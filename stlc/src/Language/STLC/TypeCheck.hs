{-# LANGUAGE LambdaCase
           , ConstraintKinds
           , FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , ViewPatterns
           , DeriveGeneric
           , DeriveDataTypeable
           , TypeSynonymInstances
           , OverloadedStrings
          #-}
module Language.STLC.TypeCheck where

import Language.STLC.Syntax
import Language.STLC.Pretty

import Control.Monad.Report

-- Type Inference
--   Type inference will enrich the ast with type annotations.
-- While this language is intended to be the target of a
-- higher level language, type inference is necessary
-- for generating tests.
import Language.Syntax.Location
import Language.STLC.Syntax

import Control.Monad.Reader

import Data.Bifunctor
import Data.List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Unbound.Generics.LocallyNameless

import Data.Text.Prettyprint.Doc

-----------------------------------------------------------------
-- Environment and Tc Monad
-----------------------------------------------------------------

data Env = Env { envVars :: Map String Type
               , envLoc  :: Loc
               } deriving (Show)

data TcErr
  = UnificationFailure Loc Type Type
  | IntLitMismatch Loc Type Int
  | FpLitMismatch Loc Type Double
  | UntypedVariable Loc String
  | IndexMismatch Loc Exp Type
  | RefMismatch Loc Type Exp
  | DerefMismatch Loc Type Exp
  | UnexpectedTuple Loc Type Exp
  | UnknownTypeErr

instance Pretty TcErr where
  pretty = \case
    UnificationFailure l t1 t2 ->
      vsep [ line <> pretty l <+> "error:" 
           , indent 4 $ vsep
                  [ "Type mismatch."
                  , "Expected:" <+> pretty t1
                  , "Actual:" <+> pretty t2]
           , line
           ]

    IntLitMismatch l t i ->
      vsep [ line <> pretty l <+> "error:" 
           , indent 4 $ vsep
                [ "Integer literal type mismatch!"
                , "Expected integer literal to have integer or float type."
                , "Actual:" <+> pretty t 
                , "in:" <+> pretty i ]
           , line
           ]

    FpLitMismatch l t d ->
      vsep [ line <> pretty l <+> "error:" 
           , indent 4 $ vsep
                [ "Floating-point literal type mismatch!"
                , "Expected float literal to have float type."
                , "Actual:" <+> pretty t 
                , "in:" <+> pretty d ]
           , line
           ]

    UntypedVariable l n ->
      vsep [ line <> pretty l <+> "error:" 
           , indent 4 $ vsep
                [ "Untyped variable encountered: " <+> pretty (show n)
                , "All top-level variables and functions must be given type signatures." 
                ]
           , line
           ]

    IndexMismatch l e ty ->
      vsep [ line <> pretty l <+> "error:" 
           , indent 4 $ vsep
                [ "Element index mismatch!"
                , "Cannot access index of non-pointer type!"
                , "Actual:" <+> pretty ty
                , "in"
                , indent 2 $ pretty e
                ]
           , line
           ]

    RefMismatch l ty e ->
      vsep [ line <> pretty l <+> "error:" 
           , indent 4 $ vsep
                [ "Expected non-pointer type, but references have pointer types."
                , "Expected:" <+> pretty ty
                , "in:" <+> pretty e ]
           , line
           ]

    DerefMismatch l ty e ->
      vsep [ line <> pretty l <+> "error:" 
           , indent 4 $ vsep
                [ "Cannot dereference non-pointer type."
                , "Type given:" <+> pretty ty
                , "in:"
                , indent 4 $ pretty e
                ]
           , line
           ]

    UnexpectedTuple l ty e ->
      vsep [ line <> pretty l <+> "error:" 
           , indent 4 $ vsep
                [ "Unexpected tuple encountered!"
                , "Expected:" <+> pretty ty
                , "in:"
                , indent 4 $ pretty e
                ]
           , line
           ]

    UnknownTypeErr ->
      "You've encountered an unknown type error!"


type Tc = ReportT TcErr (ReaderT Env FreshM)
type MonadTc m = (MonadReport TcErr m, MonadReader Env m, Fresh m)

-- helpers

envInsertType :: String -> Type -> Env -> Env
envInsertType n ty env = env { envVars = Map.insert n ty (envVars env) }

envUpdateLocation :: Loc -> Env -> Env
envUpdateLocation l env = env { envLoc = l }

lookupType :: MonadTc m => String -> m Type
lookupType n
  = reader (Map.lookup n . envVars) >>= maybe err return
  where err = do
          l <- getLoc
          fatal $ UntypedVariable l n

getLoc :: MonadTc m => m Loc
getLoc = reader envLoc

withType :: MonadTc m => String -> Type -> m a -> m a
withType n ty = local (envInsertType n ty)

withTypes :: MonadTc m => [(String, Type)] -> m a -> m a
withTypes ns = local (\env -> foldl f env ns)
  where f env (n, ty) = envInsertType n ty env 

withLoc :: MonadTc m => Loc -> m a -> m a
withLoc l = local (envUpdateLocation l)

withMayLoc :: MonadTc m => Maybe Loc -> m a -> m a
withMayLoc Nothing = id
withMayLoc (Just l) = withLoc l

-----------------------------------------------------------------
-- Module Typechecking
-----------------------------------------------------------------

checkModule :: Module -> Either [TcErr] Module
checkModule (Module l n defns)
  = second (Module l n) edefs
  where env = makeEnv l defns
        m = checkDefns defns
        edefs = runFreshM $ runReaderT (runReportT m) env

makeEnv :: Loc -> [Defn] -> Env
makeEnv l defns = Env { envVars = Map.fromList defns''
                      , envLoc = l
                      }
  where defns' = concatMap f defns
        defns'' = nubBy (\(n1,_) (n2, _) -> if n1 == n2 then err n1 else False) defns'
        err n = error $ "Illegal duplicate definition: " ++ n
        f = \case
          FuncDefn (Func _ ty f bnd) -> [(f, ty)]
          
          ExternDefn (Extern _ n paramty retty) ->
            [(n, tarr paramty retty)]

          DataTypeDefn (DataType _ dt_n dt_cons) ->
            let ens = mconcat (getEntries <$> dt_cons)
                mem_tys = [ (n, TArr (TCon dt_n) ty) | Entry _ n ty <- ens ]
                con_tys = do
                  ConstrDefn _ n con_params <- dt_cons
                  let ty = tarr con_params (TCon dt_n)
                  return (n, ty)
                rcd_tys = do
                  RecordDefn _ n ens <- dt_cons
                  let tys = [ty | Entry _ _ ty <- NE.toList ens]
                      ty = tarr tys (TCon n)
                  return (n, ty)
            in mem_tys ++ con_tys ++ rcd_tys



-----------------------------------------------------------------
-- Bidirectional Type Checking
-----------------------------------------------------------------

checkDefns :: MonadTc m => [Defn] -> m [Defn]
checkDefns = mapM checkDefn

checkDefn :: MonadTc m => Defn -> m Defn
checkDefn = \case
  FuncDefn (Func l ty f bnd) -> withLoc l $ do
    (ps, body) <- unbind bnd

    let (argtys, retty) = splitType ty
    ps' <- mapM (\(p, ty) -> tcPat p (Just ty)) (zip ps argtys)
    let paramtys = concatMap patTypedVars ps' 
    body' <- withTypes paramtys $ checkType body retty

    let bnd' = bind ps' body'
    return $ FuncDefn $ Func l ty f bnd'

  defn -> return defn


-- | Infer the type of a term, producing an annotated version of the 
-- term (whose type can *always* be inferred).
inferType :: MonadTc m => Exp -> m Exp
inferType e = tcExp e Nothing

-- | Check that the given term has the expected type.  
-- The provided type does not necessarily need to be in whnf, but it should be
-- elaborated (i.e. already checked to be a good type).
checkType :: MonadTc m => Exp -> Type -> m Exp
checkType e expectedTy = tcExp e (Just expectedTy)

tcExp :: MonadTc m => Exp -> Maybe Type -> m Exp
-- Expression Variables
tcExp e@(EVar v) Nothing = EType e <$> lookupType (name2String v)
tcExp e@(EVar v) (Just t1) = do
  t2 <- lookupType (name2String v)
  EType e <$> unify t1 t2

-- Expression Literals
tcExp (ELit l) mty = tcLit l mty

-- Expression Application
tcExp (EApp f xs) Nothing = do
    f' <- inferType f
    let (NE.fromList -> paramtys, retty) = splitType $ exType f'
    xs' <- mapM (\(x, xty) -> checkType x xty) (NE.zip xs paramtys)
    return $ EType (EApp f' xs') retty

tcExp (EApp f xs) (Just ty) = do
  f' <- inferType f
  let (NE.fromList -> paramtys, retty) = splitType $ exType f'
  xs' <- mapM (\(x, xty) -> checkType x xty) (NE.zip xs paramtys)
  EType (EApp f' xs') <$> unify ty retty

-- Expression Type Annotations
tcExp (EType e ty') Nothing = checkType e ty'
tcExp (EType e ty') (Just ty) = checkType e =<< unify ty ty'

-- Expression Type Casts
tcExp (ECast e ty') Nothing
  = EType <$> (ECast <$> inferType e <*> pure ty') <*> pure ty'
tcExp (ECast e ty') (Just ty) = do
  ty'' <- unify ty ty'
  EType <$> (ECast <$> inferType e <*> pure ty'') <*> pure ty''

tcExp (ELoc e l) mty = withLoc l $ ELoc <$> tcExp e mty <*> pure l
tcExp (EParens e) mty = EParens <$> tcExp e mty

-- Lambda Expressions
tcExp (ELam bnd) Nothing = do
  (NE.toList -> ps, body) <- unbind bnd
  ps' <- mapM (\p -> tcPat p Nothing) ps
  let ns = concatMap patTypedVars ps'
  body' <- withTypes ns (inferType body)
  let retty = exType body'
      paramtys = exPType <$> ps'
      ty' = tarr paramtys retty
  return $ EType (ELam $ bind (NE.fromList ps') body') ty'

tcExp (ELam bnd) (Just ty) = do
  let (paramtys, retty) = splitType ty
  (NE.toList -> ps, body) <- unbind bnd
  when (length paramtys /= length ps)
     $ error $ "tcExp - Lambda type arity mismatch!"

  ps' <- mapM (\(p, pty) -> tcPat p (Just pty)) (zip ps paramtys)
  let ns = concatMap patTypedVars ps'
  body' <- withTypes ns (checkType body retty)
  let retty' = exType body'
      paramtys' = exPType <$> ps'
      ty' = tarr paramtys' retty'
  EType (ELam $ bind (NE.fromList ps') body')
    <$> unify ty ty'

-- Let Expression
tcExp (ELet bnd) mty = do
  (unrec -> letbnds, body) <- unbind bnd
  let qs = second unembed <$> letbnds
      go [] qs' = return qs'
      go (q:qs) qs' = do
            q'@(p, _) <- tcLetBind q
            let ns = patTypedVars p
            withTypes ns $ go qs (q':qs')
  qs' <- reverse <$> go (NE.toList qs) []
  let ns = concatMap (patTypedVars . fst) qs'
  withTypes ns $ do
    body' <- tcExp body mty
    let letbnds' = rec $ NE.fromList (second embed <$> qs')
    return $ EType (ELet $ bind letbnds' body')
                   (exType body')

tcExp (EIf p t f) Nothing = do
  p' <- checkType p TBool
  t' <- inferType t
  f' <- tcElse f Nothing
  ty <- unify (exType t') (exElseType f')
  return $ EType (EIf p' t' f') ty


tcExp (EIf p t f) (Just ty) = do
  p' <- checkType p TBool
  t' <- checkType t ty
  f' <- tcElse f (Just ty)
  return $ EType (EIf p' t' f') ty

tcExp (ECase e cls) mty = do
  e' <- tcExp e Nothing
  let ety = exType e'
  cls' <- mapM (\cl -> tcClause cl (Just ety) mty) cls
  
  ty' <- exType <$> exClauseBody (NE.head cls')
  return $ EType (ECase e' cls') ty'

-- Ref Expression
tcExp (ERef e) Nothing = do
  e' <- inferType e
  let ty' = TPtr $ exType e'
  return $ EType (ERef e') ty'

tcExp (ERef e) (Just ty) = do
  let ty' = exTyAnn ty
  case ty' of
    TPtr ety -> do
      e' <- checkType e ety
      return $ EType (ERef e') ty
    _ -> do
      l <- getLoc
      fatal $ RefMismatch l ty (ERef e)


-- Deref Expression
tcExp (EDeref e) Nothing = do
  e' <- inferType e
  let ty = exTyAnn $ exType e'
  case ty of
    TPtr ty' -> return $ EType (EDeref e') ty'
    _ -> do
      l <- getLoc
      fatal $ DerefMismatch l ty (EDeref e)

tcExp (EDeref e) (Just ty) = do
  e' <- checkType e (TPtr ty)
  return $ EType (EDeref e') ty

tcExp (ETuple e es) Nothing = do
  e' <- inferType e
  es' <- mapM inferType es
  let ty = exType e'
      tys = exType <$> es'
  return $ EType (ETuple e' es') (TTuple ty tys)

tcExp (ETuple e es) (Just ty1) = do
  case exTyAnn ty1 of
    TTuple ty2 ty2s -> do
      e' <- checkType e ty2
      es' <- mapM (uncurry checkType) (NE.zip es ty2s)
      return $ EType (ETuple e' es') ty1

    _ -> do
      l <- getLoc
      fatal $ UnexpectedTuple l ty1 (ETuple e es)



-- Constructor Expression
tcExp (ECon n args) mty = do
  (paramtys, retty) <- splitType <$> lookupType n
  e' <- ECon n <$> zipWithM (\arg argty -> tcExp arg argty)
                            args (Just <$> paramtys)
  ty' <- maybe (return retty) (`unify` retty) mty
  return $ EType e' ty'

-- New Expression
tcExp (ENewCon n args) mty = do
  (paramtys, retty) <- (second TPtr . splitType) <$> lookupType n
  e' <- ENewCon n <$> zipWithM (\arg argty -> tcExp arg argty)
                               args (Just <$> paramtys)
  ty' <- maybe (return retty) (`unify` retty) mty
  return $ EType e' ty'

-- Free Expression
tcExp (EFree e) mty =
  EType <$> (EFree <$> tcExp e mty) <*> pure TI8

tcExp (EGet e n) mty = do
  mget_ty <- lookupType n
  let (ety, ty') = first (head) $ splitType mget_ty
  e' <- checkType e ety
  EType (EGet e' n) <$> maybe (return ty') (`unify` ty') mty

tcExp (EGetI e i) mty = do
  i' <- checkType i TI32
  e' <- tcExp e (TPtr <$> mty)
  let ty = exType e'
  case exTyAnn ty of
    TPtr     ty' -> return $ EType (EGetI e' i') ty'
    TArray _ ty' -> return $ EType (EGetI e' i') ty'
    _ -> do
      l <- getLoc
      fatal $ IndexMismatch l (EGetI e' i') ty


tcExp (ESet lhs rhs) mty = do
  lhs' <- maybe (inferType lhs) (checkType lhs) mty
  rhs' <- checkType rhs (exType lhs')
  return $ EType (ESet lhs' rhs') (exType rhs')


tcExp (ENewArray []) Nothing =
    error $ "Cannot infer type of new array, please provide type annotations."

tcExp (ENewArray (e:es)) Nothing = do
  e' <- inferType e
  let ety = exType e'
  es' <- mapM (`checkType` ety) es
  return $ EType (ENewArray (e':es')) (TPtr ety)

tcExp (ENewArray es) (Just ty@(TPtr ety)) = do
  es' <- mapM (`checkType` ety) es
  return $ EType (ENewArray es') (TPtr ety)

tcExp (ENewArray es) (Just ty@(TArray i ety))
  | length es /= i = error $ "Array type and size mismatch!"
  | otherwise = do
      es' <- mapM (`checkType` ety) es
      return $ EType (ENewArray es') (TPtr ety)

tcExp (ENewArray es) (Just ty)
  = error $ "Type mismatch!\n\n"
         ++ "Expected: " ++ show ty ++ "\n"
         ++ "Actual: *Array or **" ++ "\n\n"

tcExp (ENewArrayI i) Nothing =
  error $ "Cannot infer type of new array, please provide type annotations."

-- Can't determine the value of i unless it's a constant.
-- So can't return array, it has to be a double pointer.
-- We'll solve this problem some other day.

tcExp (ENewArrayI i) (Just ty@(exTyAnn -> TPtr _)) = do
  i' <- checkType i TI32
  return $ EType (ENewArrayI i') ty

tcExp (ENewArrayI i) (Just ty)
  = error $ "Type mismatch on new array!\n"
         ++ "Expected: " ++ show ty ++ "\n"
         ++ "Actual: *x   (for some unknown x)\n\n"

tcExp (EResizeArray e i) Nothing = do  
  e' <- inferType e
  i' <- checkType i TI32
  let ty = exType e'
  case exType e' of
    TPtr (TPtr _) ->
      return $ EType (EResizeArray e' i') ty
    
    TPtr (TArray _ ety) ->
      return $ EType (EResizeArray e' i') (TPtr $ TPtr ety)

    _ -> error $ "Type mismatch on resize array!\n\n"
              ++ "Expected: Pointer to array or pointer\n\n"
              ++ "Actual: " ++ show ty

tcExp (EResizeArray e i) (Just ty@(TPtr (TPtr ety))) = do  
  e' <- checkType e ety
  i' <- checkType i TI32
  return $ EType (EResizeArray e' i') ty

tcExp (EResizeArray e i) (Just ty)
  = error $ "Type mismatch on new array!\n\n"
         ++ "Expected: " ++ show ty ++ "\n\n"
         ++ "Actual: **x   (for some unknown x)"


tcExp (ENewStringI i) Nothing = do
  i' <- checkType i TI32
  return $ EType (ENewStringI i') (TPtr TI8)

tcExp (ENewStringI i) (Just ty) = do
  i' <- checkType i TI32
  EType (ENewStringI i') <$> unify ty (TPtr TI8)


-- Expression Operations
tcExp (EOp op) mty = tcOp op mty


-- Type checking on literals
tcLit :: MonadTc m => Lit -> Maybe Type -> m Exp

tcLit LNull Nothing = error "Cannot infer null"

tcLit LNull (Just t1@(exTyAnn -> TPtr _)) =
  return $ EType (ELit LNull) t1

tcLit LNull (Just t1) = do
  l <- getLoc
  fatal $ UnificationFailure l t1 (TPtr t1)


tcLit (LInt i) Nothing = return $ EType (ELit $ LInt i) TI32
tcLit (LInt i) (Just ty)
  | isIntTy ty   = return $ EType (ELit $ LInt i) ty
  | isFloatTy ty = return $ EType (ELit $ LDouble (fromIntegral i)) ty
  | otherwise = do
      l <- getLoc
      fatal $ IntLitMismatch l ty i

tcLit (LDouble d) Nothing   = return $ EType (ELit $ LDouble d) TF64
tcLit (LDouble d) (Just ty)
  | isFloatTy ty = EType (ELit $ LDouble d) <$> unify ty ty
  | otherwise = do
      l <- getLoc
      fatal $ FpLitMismatch l ty d

tcLit (LChar c) Nothing   = return $ EType (ELit $ LChar c) TI8
tcLit (LChar c) (Just ty)= EType (ELit $ LChar c) <$> unify ty TI8

-- Strings
tcLit (LString s) Nothing
  = return $ EType (ELit $ LString s) (TPtr TI8)
tcLit (LString s) (Just ty) =
  EType (ELit $ LString s) <$> unify ty (TPtr TI8)


tcLit (LStringI i) Nothing = do
  i' <- checkType i TI32
  return $ EType (ELit $ LStringI i') (TPtr TI8)

tcLit (LStringI i) (Just ty) = do
  i' <- checkType i TI32
  EType (ELit $ LStringI i') <$> unify (TPtr TI8) ty

-- Arrays
tcLit (LArray []) Nothing
  = error $ "Unable to infer type of empty array."

tcLit (LArray (x:xs)) Nothing = do
  x' <- inferType x
  let ty = exType x'
  xs' <- mapM (\x -> checkType x ty) xs
  let n = length (x:xs)
  return $ EType (ELit $ LArray (x':xs')) (TPtr ty)

tcLit (LArray xs) (Just (TPtr ty)) = do
    xs' <- mapM (\x -> checkType x ty) xs
    return $ EType (ELit $ LArray xs') (TPtr ty) 

tcLit (LArray xs) (Just (TArray n ty))
  | length xs == n = do
      xs' <- mapM (\x -> checkType x ty) xs
      return $ EType (ELit $ LArray xs') (TPtr ty)
  
  | otherwise = error $ "Array length mismatch!"

tcLit (LArray _) (Just ty)
  = error $ "Expected Array type, found " ++ show (pretty ty)

tcLit (LArrayI _) Nothing
       
  = error "Cannot infer array type. Please provide annotations." 

tcLit (LArrayI i) (Just (TArray n ty)) = do
  i' <- checkType i TI32
  return $ EType (ELit $ LArrayI i) (TArray n ty)

tcLit (LArrayI _) (Just ty)
  = error $ "Expected Array type, found " ++ show (pretty ty)




tcClause :: MonadTc m => Clause -> Maybe Type -> Maybe Type -> m Clause
tcClause (Clause ml bnd) pmty emty = withMayLoc ml $ do
  q <- unbind bnd
  (p, e) <- tcEquation q pmty emty
  return $ Clause ml (bind p e)

tcEquation :: MonadTc m => (Pat, Exp) -> Maybe Type -> Maybe Type -> m (Pat, Exp)
tcEquation (p, e) pmty emty = do
  p' <- tcPat p pmty
  let ns = patTypedVars p'
  withTypes ns $ do
    e' <- tcExp e emty
    return (p', e')

tcLetBind :: MonadTc m => (Pat, Exp) -> m (Pat, Exp)
tcLetBind (p, e) = do
  e' <- inferType e
  let ty' = exType e'
  p' <- tcPat p (Just ty')
  return (p', e')


tcElse :: MonadTc m => Else -> Maybe Type -> m Else
tcElse (Else ml e) mty
  = withMayLoc ml $ Else ml <$> tcExp e mty
tcElse (Elif ml p t f) (Nothing) = withMayLoc ml $ do
  p' <- checkType p TBool
  t' <- inferType t
  f' <- tcElse f Nothing
  ty <- unify (exType t') (exElseType f')
  return $ ty `seq` Elif ml p' t' f'


tcElse (Elif ml p t f) (Just ty) = withMayLoc ml $
  Elif ml <$> checkType p TBool <*> checkType t ty <*> tcElse f (Just ty)




tcOp :: MonadTc m => Op -> Maybe Type -> m Exp
tcOp op mty = case op of
  OpAddI a b -> tcBOp OpAddI a b mty intTypes Nothing
  OpSubI a b -> tcBOp OpSubI a b mty intTypes Nothing
  OpMulI a b -> tcBOp OpMulI a b mty intTypes Nothing
  OpDivI a b -> tcBOp OpDivI a b mty intTypes Nothing
  OpRemI a b -> tcBOp OpRemI a b mty intTypes Nothing

  OpAddF a b -> tcBOp OpAddF a b mty floatTypes Nothing
  OpSubF a b -> tcBOp OpSubF a b mty floatTypes Nothing
  OpMulF a b -> tcBOp OpMulF a b mty floatTypes Nothing
  OpDivF a b -> tcBOp OpDivF a b mty floatTypes Nothing
  OpRemF a b -> tcBOp OpRemF a b mty floatTypes Nothing

  OpAnd a b -> tcBOp OpAnd a b mty [TBool] (Just TBool)
  OpOr  a b -> tcBOp OpOr  a b mty [TBool] (Just TBool)
  OpXor a b -> tcBOp OpXor a b mty [TBool] (Just TBool)

  OpEqI  a b -> tcBOp OpEqI  a b mty intTypes (Just TBool)
  OpNeqI a b -> tcBOp OpNeqI a b mty intTypes (Just TBool)
  
  OpLT  a b -> tcBOp OpLT a b mty intTypes (Just TBool)
  OpLE  a b -> tcBOp OpLE a b mty intTypes (Just TBool)
  OpGT  a b -> tcBOp OpGT a b mty intTypes (Just TBool)
  OpGE  a b -> tcBOp OpGE a b mty intTypes (Just TBool)


tcBOp :: MonadTc m  => (Exp -> Exp -> Op) -> Exp -> Exp -> Maybe Type
                    -> [Type] -> Maybe Type -> m Exp
tcBOp constr a b Nothing paramtys Nothing = do
  a' <- inferType a
  let aty = exType a'
  unless (aty `elem` paramtys)
        $ error $ "Operation has unexpected type:"
  b' <- checkType b aty
  let op' = constr a' b'
  return $ EType (EOp op') aty

tcBOp constr a b (Just ty) paramtys Nothing = do
  unless (ty `elem` paramtys)
       $ error $ "Operation has unexpected type"
  op' <- constr <$> checkType a ty <*> checkType b ty
  return $ EType (EOp op') ty

tcBOp constr a b Nothing paramtys (Just retty) = do
  a' <- inferType a
  let aty = exType a'
  unless (aty `elem` paramtys)
      $ error $ "Operation has unexpected type!"
  b' <- checkType b aty
  let op' = constr a' b'
  return $ EType (EOp op') retty

tcBOp constr a b (Just ty) paramtys (Just retty) = do
  a' <- inferType a
  let aty = exType a'
  unless (aty `elem` paramtys)
      $ error $ "Operation has unexpected type!"
  b' <- checkType b aty
  let op' = constr a' b'
  EType (EOp op') <$> unify ty retty


tcPat :: MonadTc m => Pat -> Maybe Type  -> m Pat
tcPat (PVar v) (Just ty) = return $ PType (PVar v) ty
tcPat (PVar v) Nothing = error $ "Cannot infer type of pattern variable " ++ name2String v

tcPat (PCon n ps) Nothing = do
  n_ty <- lookupType n
  let (argtys, retty) = splitType n_ty
  ps' <- mapM (\(p, argty) -> tcPat p (Just argty)) (zip ps argtys)
  return $ PType (PCon n ps') retty

tcPat (PCon n ps) mty = do
  n_ty <- lookupType n
  let (argtys, retty) = splitType n_ty
  ps' <- mapM (\(p, pty) -> tcPat p (Just pty)) (zip ps argtys)
  case mty of
    Just ty
      | ty /= retty -> error $ "Pattern type mismatch"
    _ -> return $ PType (PCon n ps') retty

tcPat (PTuple p ps) Nothing = do
    p' <- tcPat p Nothing
    ps' <- mapM (`tcPat` Nothing) ps 
    return $ PType (PTuple p' ps')
                   (TTuple (exPType p') (exPType <$> ps'))

tcPat (PTuple p ps) (Just ty) = do
  case exTyAnn ty of
    TTuple t ts -> do
      p' <- tcPat p (Just t)
      ps' <- mapM (uncurry tcPat) (NE.zip ps (Just <$> ts)) 
      return $ PType (PTuple p' ps') ty
    _ -> error $ "Type mismatch!\nExpected: " ++ show (pretty ty) ++ "\nActual: Some tuple\n"

tcPat PWild (Just ty) = return $ PType PWild ty
tcPat PWild Nothing = error $ "Can not infer type of wildcard pattern."

tcPat (PType p ty') (Just ty) 
  | ty == ty' = tcPat p (Just ty')
  | True = do
      env <- ask
      error $ "\nPattern type mismatch:\n"
          ++ "pattern: " ++ show (pretty p) ++ "\n\n"
          ++ "actual type: " ++ show (pretty ty) ++ "\n\n"
          ++ "expected type: " ++ show (pretty ty') ++ "\n\n"
          ++ "env: " ++ show env ++ "\n\n"

tcPat (PType p ty') Nothing = tcPat p (Just ty')

tcPat (PLoc p l) mty = withLoc l (PLoc <$> tcPat p mty <*> pure l)
tcPat (PParens p) mty = PParens <$> tcPat p mty




------------------------------------------------------------------------------------------------
-- Unification 
------------------------------------------------------------------------------------------------

unify :: MonadTc m => Type -> Type -> m Type
unify t1 t2
  | t1 == t2 = return t2
  | otherwise = do
      l <- getLoc
      fatal $ UnificationFailure l t1 t2