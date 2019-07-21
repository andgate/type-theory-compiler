{-# LANGUAGE ConstraintKinds
           , LambdaCase
           , ViewPatterns
           , FlexibleContexts
           , GeneralizedNewtypeDeriving
           , OverloadedStrings
          #-}
module Language.STLC.Namecheck where


import Language.Syntax.Location
import Language.STLC.Syntax

import Control.Monad.Reader
import Control.Monad.Report
import Data.Bifunctor
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NE

import Unbound.Generics.LocallyNameless
import Data.Text.Prettyprint.Doc

--- Namecheck Environment
data Env = Env
  { envNames :: Map SName Loc
  , envLoc :: Loc  
  }

mkEnv :: [(SName, Loc)] -> Loc -> Env
mkEnv ns l
  = Env { envNames = Map.fromList ns
        , envLoc = l
        }

envInsertNames :: [(String, Loc)] -> Env -> Env
envInsertNames ns env
  = env { envNames = Map.union (Map.fromList $ first (`SName` VarName) <$> ns)
                               (envNames env)
        }
                          

--- Namecheck Errors

data Err
  = UndefinedName Loc String
  | UndefinedConstructor Loc String
  | UndefinedTypeConstructor Loc String


instance Pretty Err where
  pretty = \case
    UndefinedName l n ->
      vsep [ line <> pretty l <+> "error:"
           , indent 4 $ "Undefined variable encountered: " <+> pretty n
           , line
           ]

    UndefinedConstructor l n ->
      vsep [ line <> pretty l <+> "error:"
           , indent 4 $ "Undefined constructor encountered: " <+> pretty n
           , line
           ]

    UndefinedTypeConstructor l n ->
      vsep [ line <> pretty l <+> "error:"
           , indent 4 $ "Undefined type constructor encountered: " <+> pretty n
           , line
           ]

-- Namecheck Monad
type MonadNamecheck m = (Fresh m, MonadReader Env m, MonadReport Err m)

newtype NamecheckT a = NC { unNamecheck :: ReportT Err (FreshMT (Reader Env)) a }
  deriving (Functor, Applicative, Monad, Fresh, MonadReader Env, MonadReport Err)

runNamecheck :: NamecheckT a -> Env-> Either [Err] a
runNamecheck m env
  = runReader (runFreshMT (runReportT (unNamecheck m))) env

withNames :: MonadNamecheck m => [(String, Loc)] -> m a -> m a
withNames dict = local (envInsertNames dict)

withLoc :: MonadNamecheck m => Loc -> m a -> m a
withLoc l = local (\env -> env { envLoc = l })

checkVar :: MonadNamecheck m => Var -> m Var
checkVar v = checkName (name2String v) >> return v

checkName  :: MonadNamecheck m => String -> m String
checkName n = do
  may_n <- (Map.lookup (SName n VarName) . envNames) <$> ask
  l <- envLoc <$> ask
  case may_n of
    Nothing -> nonfatal (UndefinedName l n) n
    Just _ -> return n


checkCon   :: MonadNamecheck m => String -> m String
checkCon n = do
  may_n <- (Map.lookup (SName n ConName) . envNames) <$> ask
  l <- envLoc <$> ask
  case may_n of
    Nothing -> nonfatal (UndefinedConstructor l n) n
    Just _ -> return n

checkTyCon :: MonadNamecheck m => String -> m String
checkTyCon n = do
  may_n <- (Map.lookup (SName n TyConName) . envNames) <$> ask
  l <- envLoc <$> ask
  case may_n of
    Nothing -> nonfatal (UndefinedTypeConstructor l n) n
    Just _ -> return n


-- Namecheck requires some global name sets and a module to check on
namecheck :: [(SName, Loc)] -> Module -> Either [Err] Module
namecheck ns m
  = runNamecheck (allFatal $ namecheckModule m)
                 (mkEnv ns (locOf m))


namecheckModule :: MonadNamecheck m => Module -> m Module
namecheckModule (Module l n defns) =
  Module l n <$> mapM namecheckDefn defns

namecheckDefn :: MonadNamecheck m => Defn -> m Defn
namecheckDefn = \case
  FuncDefn     f -> FuncDefn <$> namecheckFunc f
  ExternDefn   ex -> ExternDefn <$> namecheckExtern ex
  DataTypeDefn dt -> DataTypeDefn <$> namecheckDataType dt


namecheckFunc :: MonadNamecheck m => Func -> m Func
namecheckFunc (Func l ty n bnd) = do
  ty' <- namecheckType ty
  (args, body) <- unbind bnd
  args' <- mapM namecheckPat args
  let ns = concatMap patVars args
  body' <- withNames ns $ namecheckExp body
  return $ Func l ty' n (bind args' body')

namecheckExtern :: MonadNamecheck m => Extern -> m Extern
namecheckExtern (Extern l n argtys retty) = withLoc l $ do
  argtys' <- mapM namecheckType argtys
  retty' <- namecheckType retty
  return $ Extern l n argtys' retty'


namecheckDataType :: MonadNamecheck m => DataType -> m DataType
namecheckDataType (DataType l n constrs)
  = withLoc l $
      DataType l n <$> mapM namecheckConstrDefn constrs

namecheckConstrDefn :: MonadNamecheck m => ConstrDefn -> m ConstrDefn
namecheckConstrDefn = \case
  ConstrDefn l n tys -> ConstrDefn l n <$> (withLoc l $ mapM namecheckType tys)
  RecordDefn l n es -> RecordDefn l n <$> (withLoc l $ mapM namecheckEntry es)

namecheckEntry :: MonadNamecheck m => Entry -> m Entry
namecheckEntry (Entry l n ty) = Entry l n <$> (withLoc l $ namecheckType ty)


namecheckType :: MonadNamecheck m => Type -> m Type
namecheckType = \case
  TArr t1 t2 -> TArr <$> namecheckType t1 <*> namecheckType t2
  TCon n -> TCon <$> checkTyCon n
  
  TInt  i -> pure $ TInt  i
  TUInt i -> pure $ TUInt i
  TFp   i -> pure $ TFp   i
  
  TTuple t ts ->
    TTuple <$> namecheckType t <*> mapM namecheckType ts

  TArray i ty ->
    TArray i <$> namecheckType ty

  TVect i ty ->
    TVect i <$> namecheckType ty
  
  TPtr ty -> TPtr <$> namecheckType ty
  TLoc ty l -> withLoc l (TLoc <$> namecheckType ty <*> pure l)
  TParens ty -> TParens <$> namecheckType ty


namecheckExp :: MonadNamecheck m => Exp -> m Exp
namecheckExp = \case
  EVar v -> EVar <$> (checkVar v)
  
  ELit l -> ELit <$> namecheckLit l
  EApp f xs -> EApp <$> namecheckExp f <*> mapM namecheckExp xs
  
  EType e ty -> EType <$> namecheckExp e <*> namecheckType ty
  ECast e ty -> ECast <$> namecheckExp e <*> namecheckType ty
  ELoc e l   -> withLoc l $ ELoc <$> namecheckExp e <*> pure l
  EParens e  -> EParens <$> namecheckExp e

  ELam bnd -> do
    (ps, body) <- unbind bnd
    ps' <- mapM namecheckPat ps
    body' <- withNames (concatMap patVars (NE.toList ps')) $ namecheckExp body
    return $ ELam (bind ps' body')
    
  ELet bnd -> do
    (unrec -> qs, body) <- unbind bnd
    let ps = fst <$> qs
        es = (unembed . snd) <$> qs
    ps' <- mapM namecheckPat ps
    withNames (concatMap patVars ps') $ do
      es' <- mapM namecheckExp es
      body' <- namecheckExp body
      return $ ELet (bind (rec $ NE.zip ps' (embed <$> es')) body') 


  EIf p t f -> EIf <$> namecheckExp p <*> namecheckExp t <*> namecheckElse f
  ECase e cs -> ECase <$> namecheckExp e <*> mapM namecheckClause cs

  ERef e -> ERef <$> namecheckExp e
  EDeref e -> EDeref <$> namecheckExp e

  ETuple e es -> ETuple <$> namecheckExp e <*> mapM namecheckExp es
  ECon n es ->
    ECon <$> checkCon n <*> mapM namecheckExp es
  ENewCon n es ->
     ENewCon <$> checkCon n <*> mapM namecheckExp es
  EFree e -> EFree <$> namecheckExp e

  EGet  a str -> EGet  <$> namecheckExp a <*> pure str -- lol check member names you doofus
  EGetI a b -> EGetI <$> namecheckExp a <*> namecheckExp b
  ESet  a b -> ESet  <$> namecheckExp a <*> namecheckExp b

  ENewArray es -> ENewArray <$> mapM namecheckExp es
  ENewArrayI i -> ENewArrayI <$> namecheckExp i
  EResizeArray e i -> EResizeArray <$> namecheckExp e <*> namecheckExp i

  ENewVect es -> ENewVect <$> mapM namecheckExp es
  ENewVectI i -> ENewVectI <$> namecheckExp i

  ENewString str -> pure $ ENewString str
  EOp op -> EOp <$> namecheckOp op


namecheckLit :: MonadNamecheck m => Lit -> m Lit
namecheckLit = \case
  LNull -> pure LNull
  LBool b -> pure $ LBool b
  LInt i -> pure $ LInt i
  LDouble i -> pure $ LDouble i
  LChar c -> pure $ LChar c
  LString str -> pure $ LString str
  LArray es -> LArray <$> mapM namecheckExp es
  LArrayI i -> LArrayI <$> namecheckExp i
  LVect es -> LVect <$> mapM namecheckExp es
  LVectI i -> LVectI <$> namecheckExp i

namecheckElse :: MonadNamecheck m => Else -> m Else
namecheckElse = \case
  Else may_l body -> do
    l <- maybe (envLoc <$> ask) pure may_l
    withLoc l $ Else (Just l) <$> namecheckExp body

  Elif may_l p t f -> do
    l <- maybe (envLoc <$> ask) pure may_l
    withLoc l $ Elif (Just l) <$> namecheckExp p <*> namecheckExp t <*> namecheckElse f


namecheckOp :: MonadNamecheck m => Op -> m Op
namecheckOp = \case
  OpAdd a b -> OpAdd <$> namecheckExp a <*> namecheckExp b
  OpSub a b -> OpSub <$> namecheckExp a <*> namecheckExp b
  OpMul a b -> OpMul <$> namecheckExp a <*> namecheckExp b
  OpDiv a b -> OpDiv <$> namecheckExp a <*> namecheckExp b
  OpRem a b -> OpRem <$> namecheckExp a <*> namecheckExp b
  OpNeg a  -> OpNeg <$> namecheckExp a


  OpAnd a b -> OpAnd <$> namecheckExp a <*> namecheckExp b
  OpOr  a b -> OpOr  <$> namecheckExp a <*> namecheckExp b
  OpXor a b -> OpXor <$> namecheckExp a <*> namecheckExp b

  OpShR a b -> OpShR <$> namecheckExp a <*> namecheckExp b
  OpShL a b -> OpShL <$> namecheckExp a <*> namecheckExp b

  OpEq  a b -> OpEq  <$> namecheckExp a <*> namecheckExp b
  OpNeq a b -> OpNeq <$> namecheckExp a <*> namecheckExp b

  OpLT a b -> OpLT <$> namecheckExp a <*> namecheckExp b
  OpLE a b -> OpLE <$> namecheckExp a <*> namecheckExp b

  OpGT a b -> OpGT <$> namecheckExp a <*> namecheckExp b
  OpGE a b -> OpGE <$> namecheckExp a <*> namecheckExp b


namecheckPat :: MonadNamecheck m => Pat -> m Pat
namecheckPat = \case
  PVar v -> return $ PVar v
  PCon n ps ->
    PCon <$> checkCon n <*> mapM namecheckPat ps
  
  PTuple p ps ->
    PTuple <$> namecheckPat p <*> mapM namecheckPat ps

  PWild -> pure PWild

  PType p ty ->
    PType <$> namecheckPat p <*> namecheckType ty
  
  PLoc p l -> withLoc l (PLoc <$> namecheckPat p <*> pure l)
  PParens p -> PParens <$> namecheckPat p


namecheckClause :: MonadNamecheck m => Clause -> m Clause
namecheckClause (Clause may_l bnd) = do
  l <- maybe (envLoc <$> ask) pure may_l
  withLoc l $ do
    (p, e) <- unbind bnd
    p' <- namecheckPat p
    e' <- withNames (patVars p') $ namecheckExp e
    return $ Clause may_l (bind p' e')