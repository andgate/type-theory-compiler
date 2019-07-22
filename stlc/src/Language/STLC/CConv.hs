{-# LANGUAGE ConstraintKinds
           , LambdaCase
           , ViewPatterns
           , FlexibleContexts
           , GeneralizedNewtypeDeriving
           , OverloadedStrings
          #-}
module Language.STLC.CConv where

import Language.STLC.Syntax

import Lens.Micro.Platform
import Control.Monad.Reader
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NE

import Unbound.Generics.LocallyNameless

--- CConv Environment
data Env = Env { envNames :: Set String }

envEmpty :: Env
envEmpty = Env { envNames = mempty }

envInsertNames :: [String] -> Env -> Env
envInsertNames ns env
  = env { envNames = Set.union (Set.fromList ns)
                               (envNames env)
        }
         

-- CConv Monad
type MonadCConv m = (Fresh m, MonadReader Env m)

newtype CConv a = NC { unCConv :: FreshMT (Reader Env) a }
  deriving (Functor, Applicative, Monad, Fresh, MonadReader Env)

runCConv :: CConv a -> a
runCConv m
  = runReader (runFreshMT (unCConv m)) envEmpty

withNames :: MonadCConv m => [String] -> m a -> m a
withNames ns = local (envInsertNames ns)

-- checkVar :: MonadCConv m => Var -> m Var
-- checkVar v = checkName (name2String v) >> return v

-- checkName  :: MonadCConv m => String -> m String
-- checkName n = do
--   may_n <- (Map.lookup (SName n VarName) . envNames) <$> ask
--   l <- envLoc <$> ask
--   case may_n of
--     Nothing -> nonfatal (UndefinedName l n) n
--     Just _ -> return n


-- CConv should transform a module without any errors
cconv :: Module -> Module
cconv m = runCConv (cconvModule m) 


cconvModule :: MonadCConv m => Module -> m Module
cconvModule (Module l n defns) =
  Module l n <$> mapM cconvDefn defns

cconvDefn :: MonadCConv m => Defn -> m Defn
cconvDefn = \case
  FuncDefn     f -> FuncDefn <$> cconvFunc f
  ExternDefn   ex -> ExternDefn <$> cconvExtern ex
  DataTypeDefn dt -> DataTypeDefn <$> cconvDataType dt


cconvFunc :: MonadCConv m => Func -> m Func
cconvFunc (Func l ty n bnd) = do
  ty' <- cconvType ty
  (args, body) <- unbind bnd
  args' <- mapM cconvPat args
  let ns = fst <$> concatMap patVars args
  body' <- withNames ns $ cconvExp body
  return $ Func l ty' n (bind args' body')

cconvExtern :: MonadCConv m => Extern -> m Extern
cconvExtern (Extern l n argtys retty) = do
  argtys' <- mapM cconvType argtys
  retty' <- cconvType retty
  return $ Extern l n argtys' retty'


cconvDataType :: MonadCConv m => DataType -> m DataType
cconvDataType (DataType l n constrs)
  = DataType l n <$> mapM cconvConstrDefn constrs

cconvConstrDefn :: MonadCConv m => ConstrDefn -> m ConstrDefn
cconvConstrDefn = \case
  ConstrDefn l n tys -> ConstrDefn l n <$> mapM cconvType tys
  RecordDefn l n es -> RecordDefn l n <$> mapM cconvEntry es

cconvEntry :: MonadCConv m => Entry -> m Entry
cconvEntry (Entry l n ty) = Entry l n <$> cconvType ty


cconvType :: MonadCConv m => Type -> m Type
cconvType = \case
  TArr t1 t2 -> TArr <$> cconvType t1 <*> cconvType t2
  TCon n -> TCon <$> pure n
  
  TInt  i -> pure $ TInt  i
  TUInt i -> pure $ TUInt i
  TFp   i -> pure $ TFp   i
  
  TTuple t ts ->
    TTuple <$> cconvType t <*> mapM cconvType ts

  TArray i ty ->
    TArray i <$> cconvType ty

  TVect i ty ->
    TVect i <$> cconvType ty
  
  TPtr ty -> TPtr <$> cconvType ty
  TLoc ty l -> TLoc <$> cconvType ty <*> pure l
  TParens ty -> TParens <$> cconvType ty


cconvExp :: MonadCConv m => Exp -> m Exp
cconvExp = \case
  EVar v -> EVar <$> pure v
  
  ELit l -> ELit <$> cconvLit l
  EApp f xs -> EApp <$> cconvExp f <*> mapM cconvExp xs
  
  EType e ty -> EType <$> cconvExp e <*> cconvType ty
  ECast e ty -> ECast <$> cconvExp e <*> cconvType ty
  ELoc e l   -> ELoc <$> cconvExp e <*> pure l
  EParens e  -> EParens <$> cconvExp e

  ELam bnd -> do
    locals <- envNames <$> ask
    (ps, body) <- unbind bnd
    ps' <- mapM cconvPat ps
    let lam_free = name2String <$> (toListOf fv bnd :: [Var])
        closure_ns = Set.intersection locals (Set.fromList lam_free)
        closure_vars = s2n <$> Set.toList closure_ns
        e_cl = case closure_vars of
                        [] -> ELit LNull
                        [x] -> EVar x
                        x:xs -> ETuple (EVar x) (EVar <$> NE.fromList xs)

        p_cl = case closure_vars of
                        [] -> PWild
                        [x] -> PVar x
                        x:xs -> PTuple (PVar x) (PVar <$> NE.fromList xs)
                        
    body' <- withNames (fst <$> concatMap patVars (NE.toList ps')) $ cconvExp body
    if length lam_free == 0 then
      return $ ELam (bind ps' body')
    else
      return $ EApp (ELam (bind (NE.cons p_cl ps') body')) (pure e_cl)
      
    
  ELet bnd -> do
    (unrec -> qs, body) <- unbind bnd
    let ps = fst <$> qs
        es = (unembed . snd) <$> qs
    ps' <- mapM cconvPat ps
    withNames (fst <$> concatMap patVars ps') $ do
      es' <- mapM cconvExp es
      body' <- cconvExp body
      return $ ELet (bind (rec $ NE.zip ps' (embed <$> es')) body') 


  EIf p t f -> EIf <$> cconvExp p <*> cconvExp t <*> cconvElse f
  ECase e cs -> ECase <$> cconvExp e <*> mapM cconvClause cs

  ERef e -> ERef <$> cconvExp e
  EDeref e -> EDeref <$> cconvExp e

  ETuple e es -> ETuple <$> cconvExp e <*> mapM cconvExp es
  ECon n es ->
    ECon <$> pure n <*> mapM cconvExp es
  ENewCon n es ->
     ENewCon <$> pure n <*> mapM cconvExp es
  EFree e -> EFree <$> cconvExp e

  EGet  a str -> EGet  <$> cconvExp a <*> pure str -- lol check member names you doofus
  EGetI a b -> EGetI <$> cconvExp a <*> cconvExp b
  ESet  a b -> ESet  <$> cconvExp a <*> cconvExp b

  ENewArray es -> ENewArray <$> mapM cconvExp es
  ENewArrayI i -> ENewArrayI <$> cconvExp i
  EResizeArray e i -> EResizeArray <$> cconvExp e <*> cconvExp i

  ENewVect es -> ENewVect <$> mapM cconvExp es
  ENewVectI i -> ENewVectI <$> cconvExp i

  ENewString str -> pure $ ENewString str
  EOp op -> EOp <$> cconvOp op


cconvLit :: MonadCConv m => Lit -> m Lit
cconvLit = \case
  LNull -> pure LNull
  LBool b -> pure $ LBool b
  LInt i -> pure $ LInt i
  LDouble i -> pure $ LDouble i
  LChar c -> pure $ LChar c
  LString str -> pure $ LString str
  LArray es -> LArray <$> mapM cconvExp es
  LArrayI i -> LArrayI <$> cconvExp i
  LVect es -> LVect <$> mapM cconvExp es
  LVectI i -> LVectI <$> cconvExp i


cconvElse :: MonadCConv m => Else -> m Else
cconvElse = \case
  Else may_l body ->
    Else may_l <$> cconvExp body

  Elif may_l p t f -> do
    Elif may_l <$> cconvExp p <*> cconvExp t <*> cconvElse f


cconvOp :: MonadCConv m => Op -> m Op
cconvOp = \case
  OpAdd a b -> OpAdd <$> cconvExp a <*> cconvExp b
  OpSub a b -> OpSub <$> cconvExp a <*> cconvExp b
  OpMul a b -> OpMul <$> cconvExp a <*> cconvExp b
  OpDiv a b -> OpDiv <$> cconvExp a <*> cconvExp b
  OpRem a b -> OpRem <$> cconvExp a <*> cconvExp b
  OpNeg a  -> OpNeg <$> cconvExp a


  OpAnd a b -> OpAnd <$> cconvExp a <*> cconvExp b
  OpOr  a b -> OpOr  <$> cconvExp a <*> cconvExp b
  OpXor a b -> OpXor <$> cconvExp a <*> cconvExp b

  OpShR a b -> OpShR <$> cconvExp a <*> cconvExp b
  OpShL a b -> OpShL <$> cconvExp a <*> cconvExp b

  OpEq  a b -> OpEq  <$> cconvExp a <*> cconvExp b
  OpNeq a b -> OpNeq <$> cconvExp a <*> cconvExp b

  OpLT a b -> OpLT <$> cconvExp a <*> cconvExp b
  OpLE a b -> OpLE <$> cconvExp a <*> cconvExp b

  OpGT a b -> OpGT <$> cconvExp a <*> cconvExp b
  OpGE a b -> OpGE <$> cconvExp a <*> cconvExp b


cconvPat :: MonadCConv m => Pat -> m Pat
cconvPat = \case
  PVar v -> return $ PVar v
  PCon n ps ->
    PCon <$> pure n <*> mapM cconvPat ps
  
  PTuple p ps ->
    PTuple <$> cconvPat p <*> mapM cconvPat ps

  PWild -> pure PWild

  PType p ty ->
    PType <$> cconvPat p <*> cconvType ty
  
  PLoc p l -> PLoc <$> cconvPat p <*> pure l
  PParens p -> PParens <$> cconvPat p


cconvClause :: MonadCConv m => Clause -> m Clause
cconvClause (Clause may_l bnd) = do
  (p, e) <- unbind bnd
  p' <- cconvPat p
  e' <- withNames (fst <$> patVars p') $ cconvExp e
  return $ Clause may_l (bind p' e')