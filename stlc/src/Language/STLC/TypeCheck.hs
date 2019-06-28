{-# LANGUAGE LambdaCase
           , ConstraintKinds
           , FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , ViewPatterns
           , DeriveGeneric
           , DeriveDataTypeable
           , TypeSynonymInstances
          #-}
module Language.STLC.TypeCheck where

import Language.STLC.Syntax
import Language.STLC.Pretty

-- Type Inference
--   Type inference will enrich the ast with type annotations.
-- While this language is intended to be the target of a
-- higher level language, type inference is necessary
-- for generating tests.

import Language.STLC.Syntax

import Control.Monad.Reader

import Data.Bifunctor
import Data.List

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Unbound.Generics.LocallyNameless

import Data.Text.Prettyprint.Doc

-----------------------------------------------------------------
-- Environment and Tc Monad
-----------------------------------------------------------------

type Env = Map String Type
type Tc = ReaderT Env FreshM
type MonadTc m = (MonadReader Env m, Fresh m)

-- helpers

lookupType :: MonadReader Env m => String -> m Type
lookupType n
  = reader (Map.lookup n) >>= maybe err return
  where err = error $ "Check: untyped variable encounted: " ++ n

withType :: MonadReader Env m => String -> Type -> m a -> m a
withType n ty
  = local (Map.insert n ty)

--------------------------------------------------------
withTypes :: MonadReader Env m => [(String, Type)] -> m a -> m a
withTypes ns = local (\env -> foldl f env ns)
  where f env (n, ty) = Map.insert n ty env 


-----------------------------------------------------------------
-- Module Typechecking
-----------------------------------------------------------------

checkModule :: Module -> Module
checkModule (Module n defns)
  = Module n defns'
  where env = makeEnv defns
        m = mapM checkDefn defns
        defns' = runFreshM (runReaderT m env)

makeEnv :: [Defn] -> Map String Type
makeEnv defns = Map.fromList defns''
  where defns' = concatMap f defns
        defns'' = nubBy (\(n1,_) (n2, _) -> if n1 == n2 then err n1 else False) defns'
        err n = error $ "Illegal duplicate definition: " ++ n
        f = \case
          FuncDefn (Func ty f bnd) -> [(f, ty)]
          
          ExternDefn (Extern n paramty retty) ->
            [(n, tarr paramty retty)]

          DataTypeDefn (DataType dt_n dt_cons) ->
            let mems = mconcat (snd <$> dt_cons)
                mem_tys = [ (n, TArr (TCon dt_n) ty) | (Just n, ty) <- mems ]
                con_tys = do
                  (con_n, con_params) <- dt_cons
                  let v = con_n
                      ty = tarr (snd <$> con_params) (TCon dt_n)
                  return (v, ty)
            in mem_tys ++ con_tys



-----------------------------------------------------------------
-- Bidirectional Type Checking
-----------------------------------------------------------------

checkDefn :: MonadTc m => Defn -> m Defn
checkDefn = \case
  FuncDefn (Func ty f bnd) -> do
    (ps, body) <- unbind bnd

    let (argtys, retty) = splitType ty
    ps' <- mapM (\(p, ty) -> tcPat p (Just ty)) (zip ps argtys)
    let paramtys = concatMap patTypedVars ps' 
    body' <- withTypes paramtys $ checkType body retty

    let bnd' = bind ps' body'
    return $ FuncDefn $ Func ty f bnd'

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
tcExp e@(EVar v) (Just ty) = do
  (EType e . unify ty) <$> lookupType (name2String v)

-- Expression Literals
tcExp (ELit l) mty = tcLit l mty

-- Expression Type Annotations
tcExp (EType e ty') Nothing = checkType e ty'
tcExp (EType e ty') (Just ty) = checkType e (unify ty ty')

-- Expression Application
tcExp (EApp f xs) Nothing = do
    f' <- inferType f
    let (paramtys, retty) = splitType $ exType f'
    xs' <- mapM (\(x, xty) -> checkType x xty) (zip xs paramtys)
    return $ EType (EApp f' xs') retty

tcExp (EApp f xs) (Just ty) = do
  f' <- inferType f
  let (paramtys, retty) = splitType $ exType f'
  xs' <- mapM (\(x, xty) -> checkType x xty) (zip xs paramtys)
  return $ EType (EApp f' xs') (unify ty retty)

-- Lambda Expressions
tcExp (ELam bnd) Nothing = do
  (ps, body) <- unbind bnd
  ps' <- mapM (\p -> tcPat p Nothing) ps
  let ns = concatMap patTypedVars ps'
  body' <- withTypes ns (inferType body)
  let retty = exType body'
      paramtys = exPType <$> ps'
      ty' = tarr paramtys retty
  return $ EType (ELam $ bind ps' body') ty'

tcExp (ELam bnd) (Just ty) = do
  let (paramtys, retty) = splitType ty
  (ps, body) <- unbind bnd
  when (length paramtys /= length ps)
     $ error $ "tcExp - Lambda type arity mismatch!"

  ps' <- mapM (\(p, pty) -> tcPat p (Just pty)) (zip ps paramtys)
  let ns = concatMap patTypedVars ps'
  body' <- withTypes ns (checkType body retty)
  let retty' = exType body'
      paramtys' = exPType <$> ps'
      ty' = tarr paramtys' retty'
  return $ EType (ELam $ bind ps' body') (unify ty ty')

-- Let Expression
tcExp (ELet bnd) mty = do
  (unrec -> letbnds, body) <- unbind bnd
  let qs = second unembed <$> letbnds
      go [] qs' = return qs'
      go (q:qs) qs' = do
            q'@(p, _) <- tcLetBind q
            let ns = patTypedVars p
            withTypes ns $ go qs (q':qs')
  qs' <- reverse <$> go qs []
  let ns = concatMap (patTypedVars . fst) qs'
  withTypes ns $ do
    body' <- tcExp body mty
    let letbnds' = rec (second embed <$> qs')
    return $ EType (ELet $ bind letbnds' body')
                    (exType body')

tcExp (EIf p t f) Nothing = do
  p' <- checkType p TBool
  t' <- inferType t
  f' <- tcElse f Nothing
  let ty = unify (exType t') (exElseType f')
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
  
  ty' <- exType <$> exClauseBody (head cls')
  return $ EType (ECase e' cls') ty'

-- Ref Expression
tcExp (ERef e) Nothing = do
  e' <- inferType e
  let ty' = TPtr $ exType e'
  return $ EType (ERef e') ty'

tcExp (ERef e) (Just ty@(TPtr ety)) = do
  e' <- checkType e ety
  return $ EType (ERef e') ty

tcExp (ERef e) (Just ty) =
  error $ "Cannot dereference non-pointer of type " ++ show (pretty ty) ++"\n\n"

-- Deref Expression
tcExp (EDeref e) Nothing = do
  e' <- inferType e
  case exType e' of
    TPtr ty' -> return $ EType (EDeref e') ty'
    _ -> error $ "Expected a pointer type. Cannot dereference a non-pointer."

tcExp (EDeref e) (Just ty) = do
  e' <- checkType e (TPtr ty)
  return $ EType (EDeref e') ty


-- Constructor Expression
tcExp (ECon n args) mty = do
  (paramtys, retty) <- splitType <$> lookupType n
  e' <- ECon n <$> zipWithM (\arg argty -> tcExp arg argty)
                            args (Just <$> paramtys)
  let ty' = maybe retty (`unify` retty) mty
  return $ EType e' ty'

-- New Expression
tcExp (ENewCon n args) mty = do
  (paramtys, retty) <- (second TPtr . splitType) <$> lookupType n
  e' <- ENewCon n <$> zipWithM (\arg argty -> tcExp arg argty)
                               args (Just <$> paramtys)
  let ty' = maybe retty (`unify` retty) mty
  return $ EType e' ty'

-- Free Expression
tcExp (EFree e) mty =
  EType <$> (EFree <$> tcExp e mty) <*> pure TVoid

tcExp (EGet e n) mty = do
  mget_ty <- lookupType n
  let (ety, ty') = first (head) $ splitType mget_ty
  e' <- checkType e ety
  return $ EType (EGet e' n)
                 (maybe ty' (`unify` ty') mty)

tcExp (EGetI e i) mty = do
  e' <- tcExp e Nothing
  i' <- checkType i TI32
  case exType e' of
    TArray _ rty ->
      return $ EType (EGetI e' i') (maybe rty (`unify` rty) mty)
    
    TPtr rty ->
      return $ EType (EGetI e' i') (maybe rty (`unify` rty) mty)

    _ -> error $ "Cannot index into non-array or non-ptr type"

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
  return $ EType (ENewArray (e':es')) (TPtr $ TArray (length es) ety)


tcExp (ENewArray es) (Just ty@(TPtr (TPtr ety))) = do
  es' <- mapM (`checkType` ety) es
  return $ EType (ENewArray es') (TPtr (TArray (length es) ety))

tcExp (ENewArray es) (Just ty@(TPtr (TArray i ety)))
  | length es /= i = error $ "Array type and size mismatch!"
  | otherwise = do
      es' <- mapM (`checkType` ety) es
      return $ EType (ENewArray es') ty

tcExp (ENewArray es) (Just ty)
  = error $ "Type mismatch!\n\n"
         ++ "Expected: " ++ show ty ++"\n\n"
         ++ "Actual: *Array or **"

tcExp (ENewArrayI i) Nothing =
  error $ "Cannot infer type of new array, please provide type annotations."

-- Can't determine the value of i unless it's a constant.
-- So can't return array, it has to be a double pointer.
-- We'll solve this problem some other day.
tcExp (ENewArrayI i) (Just ty@(TPtr (TPtr _))) = do
  i' <- checkType i TI32
  return $ EType (ENewArrayI i') ty

tcExp (ENewArrayI i) (Just ty)
  = error $ "Type mismatch on new array!\n\n"
         ++ "Expected: " ++ show ty ++ "\n\n"
         ++ "Actual: **x   (for some unknown x)"

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


-- Expression Operations
tcExp (EOp op) mty = tcOp op mty


-- Type checking on literals
tcLit :: MonadTc m => Lit -> Maybe Type -> m Exp

tcLit (LInt i) Nothing = return $ EType (ELit $ LInt i) TI32
tcLit (LInt i) (Just ty)
  | ty `elem` inttypes = return $ EType (ELit $ LInt i) ty
  | otherwise = error $ "Expected Int literal to have integer type. "
                     ++ "Instead, found type " ++ show (pretty ty)

tcLit (LChar c) Nothing = return $ EType (ELit $ LChar c) TChar
tcLit (LChar c) (Just TChar) = return $ EType (ELit $ LChar c) TChar
tcLit (LChar c) (Just ty)
  = error $ "Expected Char type, found " ++ show (pretty ty)

-- Strings
tcLit (LString s) Nothing
  = return $ EType (ELit $ LString s) TString
tcLit (LString s) (Just TString)
  = return $ EType (ELit $ LString s) TString
tcLit (LString s) (Just ty)
  = error $ "Expected String type, found " ++ show (pretty ty)

tcLit (LStringI i) Nothing = do
  i' <- checkType i TI32
  return $ EType (ELit $ LStringI i') TString

tcLit (LStringI i) (Just TString) = do
  i' <- checkType i TI32
  return $ EType (ELit $ LStringI i') TString

tcLit (LStringI _) (Just ty)
  = error $ "Expected String type, found " ++ show (pretty ty)

-- Arrays
tcLit (LArray xs) Nothing
  = error "Cannot infer array type. Please provide annotations." 

tcLit (LArray xs) (Just (TArray n ty)) = do
  xs' <- mapM (\x -> checkType x ty) xs
  return $ EType (ELit $ LArray xs') (TArray n ty)

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
tcClause (Clause bnd) pmty emty = do
  q <- unbind bnd
  (p, e) <- tcEquation q pmty emty
  return $ Clause (bind p e)

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
tcElse (Else e) mty = Else <$> tcExp e mty
tcElse (Elif p t f) (Nothing) = do
  p' <- checkType p TBool
  t' <- inferType t
  f' <- tcElse f Nothing
  let ty = unify (exType t') (exElseType f')
  return $ ty `seq` Elif p' t' f'


tcElse (Elif p t f) (Just ty) 
  = Elif <$> checkType p TBool <*> checkType t ty <*> tcElse f (Just ty)


tcOp :: MonadTc m => Op -> Maybe Type -> m Exp
tcOp op mty = case op of
  OpAddI a b -> tcBOp OpAddI a b mty TI32 TI32
  OpSubI a b -> tcBOp OpSubI a b mty TI32 TI32
  OpMulI a b -> tcBOp OpMulI a b mty TI32 TI32
  OpEqI  a b -> tcBOp OpEqI  a b mty TI32 TBool

  OpAddF a b -> error "Floating point unsupported"
  OpSubF a b -> error "Floating point unsupported"
  OpMulF a b -> error "Floating point unsupported"


tcBOp :: MonadTc m  => (Exp -> Exp -> Op) -> Exp -> Exp
                    -> Maybe Type -> Type -> Type -> m Exp
tcBOp constr a b Nothing paramty retty = 
  tcBOp constr a b (Just retty) paramty retty
tcBOp constr a b (Just retty') paramty retty = do
  op' <- constr <$> checkType a paramty <*> checkType b paramty
  return $ EType (EOp op') (unify retty retty')


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