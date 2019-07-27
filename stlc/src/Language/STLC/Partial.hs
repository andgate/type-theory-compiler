{-# LANGUAGE ConstraintKinds
           , LambdaCase
           , ViewPatterns
           , FlexibleContexts
           , GeneralizedNewtypeDeriving
           , OverloadedStrings
          #-}
module Language.STLC.Partial where

import Language.Syntax.Location
import Language.STLC.Syntax

import Control.Monad.Reader
import Control.Monad.Writer.CPS
import Data.Bifunctor
import Data.DList (DList)
import qualified Data.DList as DL
import qualified Data.List.NonEmpty as NE

import Unbound.Generics.LocallyNameless

--- Lambda Lift Environment
data Env = Env { envLoc :: Loc }

mkEnv :: Loc -> Env
mkEnv l = Env { envLoc = l }


-- Lambda Lifting Monad
type MonadLift m = (Fresh m, MonadReader Env m, MonadWriter (DList Func) m)

newtype Lift a = Lift { unLift :: FreshMT (WriterT (DList Func) (Reader Env)) a }
  deriving (Functor, Applicative, Monad, Fresh, MonadReader Env, MonadWriter (DList Func))

runLift :: Lift a -> Loc -> (a, [Func])
runLift m l
  = second DL.toList $ runReader (runWriterT (runFreshMT (unLift m))) (mkEnv l)

withLoc :: MonadLift m => Loc -> m a -> m a
withLoc l = local (\env -> env { envLoc = l })

withMaybeLoc :: MonadLift m => Maybe Loc -> m a -> m a
withMaybeLoc (Just l) = withLoc l
withMaybeLoc Nothing = id

writeFunc :: MonadLift m => String -> [Pat] -> Exp -> m ()
writeFunc n ps body = do
  l <- envLoc <$> ask
  let ty = tarr (exPType <$> ps) (exType body) 
  tell $ DL.singleton $ Func l ty n (bind ps body)


-- Lift should transform a module without any errors
lift :: Module -> (Module, [Func])
lift m@(Module l _ _) = runLift (liftModule m) l

liftModule :: MonadLift m => Module -> m Module
liftModule (Module l n defns) =
  Module l n <$> mapM liftDefn defns

liftDefn :: MonadLift m => Defn -> m Defn
liftDefn = \case
  FuncDefn     f -> FuncDefn <$> liftFunc f
  ExternDefn   ex -> ExternDefn <$> liftExtern ex
  DataTypeDefn dt -> DataTypeDefn <$> liftDataType dt


liftFunc :: MonadLift m => Func -> m Func
liftFunc (Func l ty n bnd) = withLoc l $ do
  ty' <- liftType ty
  (args, body) <- unbind bnd
  args' <- mapM liftPat args
  body' <- liftExp body
  return $ Func l ty' n (bind args' body')

liftExtern :: MonadLift m => Extern -> m Extern
liftExtern (Extern l n argtys retty) = withLoc l $ do
  argtys' <- mapM liftType argtys
  retty' <- liftType retty
  return $ Extern l n argtys' retty'


liftDataType :: MonadLift m => DataType -> m DataType
liftDataType (DataType l n constrs)
  = DataType l n <$> withLoc l (mapM liftConstrDefn constrs)

liftConstrDefn :: MonadLift m => ConstrDefn -> m ConstrDefn
liftConstrDefn = \case
  ConstrDefn l n tys -> ConstrDefn l n <$> withLoc l (mapM liftType tys)
  RecordDefn l n es -> RecordDefn l n <$> withLoc l (mapM liftEntry es)

liftEntry :: MonadLift m => Entry -> m Entry
liftEntry (Entry l n ty) = Entry l n <$> withLoc l (liftType ty)


liftType :: MonadLift m => Type -> m Type
liftType = \case
  TArr t1 t2 -> TArr <$> liftType t1 <*> liftType t2
  TCon n -> TCon <$> pure n
  
  TInt  i -> pure $ TInt  i
  TUInt i -> pure $ TUInt i
  TFp   i -> pure $ TFp   i
  
  TTuple t ts ->
    TTuple <$> liftType t <*> mapM liftType ts

  TArray i ty ->
    TArray i <$> liftType ty

  TVect i ty ->
    TVect i <$> liftType ty
  
  TPtr ty -> TPtr <$> liftType ty
  TLoc ty l -> withLoc l (TLoc <$> liftType ty <*> pure l)
  TParens ty -> TParens <$> liftType ty


liftExp :: MonadLift m => Exp -> m Exp
liftExp = \case
  EVar v -> EVar <$> pure v
  
  ELit l -> ELit <$> liftLit l
  EApp f xs -> EApp <$> liftExp f <*> mapM liftExp xs
  
  EType e ty -> EType <$> liftExp e <*> liftType ty
  ECast e ty -> ECast <$> liftExp e <*> liftType ty
  ELoc e l   -> withLoc l (ELoc <$> liftExp e <*> pure l)
  EParens e  -> EParens <$> liftExp e

  ELam bnd -> do
    (ps, body) <- unbind bnd
    n <- fresh $ s2n "lambda"
    ps' <- mapM liftPat ps
    body' <- liftExp body
    writeFunc (name2String n) (NE.toList ps') body'
    return $ EVar n
      
    
  ELet bnd -> do
    (unrec -> qs, body) <- unbind bnd
    let ps = fst <$> qs
        es = (unembed . snd) <$> qs
    ps' <- mapM liftPat ps
    es' <- mapM liftExp es
    body' <- liftExp body
    return $ ELet (bind (rec $ NE.zip ps' (embed <$> es')) body') 


  EIf p t f -> EIf <$> liftExp p <*> liftExp t <*> liftElse f
  ECase e cs -> ECase <$> liftExp e <*> mapM liftClause cs

  ERef e -> ERef <$> liftExp e
  EDeref e -> EDeref <$> liftExp e

  ETuple e es -> ETuple <$> liftExp e <*> mapM liftExp es
  ECon n es ->
    ECon <$> pure n <*> mapM liftExp es
  ENewCon n es ->
     ENewCon <$> pure n <*> mapM liftExp es
  EFree e -> EFree <$> liftExp e

  EGet  a str -> EGet  <$> liftExp a <*> pure str -- lol check member names you doofus
  EGetI a b -> EGetI <$> liftExp a <*> liftExp b
  ESet  a b -> ESet  <$> liftExp a <*> liftExp b

  ENewArray es -> ENewArray <$> mapM liftExp es
  ENewArrayI i -> ENewArrayI <$> liftExp i
  EResizeArray e i -> EResizeArray <$> liftExp e <*> liftExp i

  ENewVect es -> ENewVect <$> mapM liftExp es
  ENewVectI i -> ENewVectI <$> liftExp i

  ENewString str -> pure $ ENewString str
  EOp op -> EOp <$> liftOp op


liftLit :: MonadLift m => Lit -> m Lit
liftLit = \case
  LNull -> pure LNull
  LBool b -> pure $ LBool b
  LInt i -> pure $ LInt i
  LDouble i -> pure $ LDouble i
  LChar c -> pure $ LChar c
  LString str -> pure $ LString str
  LArray es -> LArray <$> mapM liftExp es
  LArrayI i -> LArrayI <$> liftExp i
  LVect es -> LVect <$> mapM liftExp es
  LVectI i -> LVectI <$> liftExp i


liftElse :: MonadLift m => Else -> m Else
liftElse = \case
  Else may_l body -> withMaybeLoc may_l $
    Else may_l <$> liftExp body

  Elif may_l p t f -> withMaybeLoc may_l $
    Elif may_l <$> liftExp p <*> liftExp t <*> liftElse f


liftOp :: MonadLift m => Op -> m Op
liftOp = \case
  OpAdd a b -> OpAdd <$> liftExp a <*> liftExp b
  OpSub a b -> OpSub <$> liftExp a <*> liftExp b
  OpMul a b -> OpMul <$> liftExp a <*> liftExp b
  OpDiv a b -> OpDiv <$> liftExp a <*> liftExp b
  OpRem a b -> OpRem <$> liftExp a <*> liftExp b
  OpNeg a  -> OpNeg <$> liftExp a


  OpAnd a b -> OpAnd <$> liftExp a <*> liftExp b
  OpOr  a b -> OpOr  <$> liftExp a <*> liftExp b
  OpXor a b -> OpXor <$> liftExp a <*> liftExp b

  OpShR a b -> OpShR <$> liftExp a <*> liftExp b
  OpShL a b -> OpShL <$> liftExp a <*> liftExp b

  OpEq  a b -> OpEq  <$> liftExp a <*> liftExp b
  OpNeq a b -> OpNeq <$> liftExp a <*> liftExp b

  OpLT a b -> OpLT <$> liftExp a <*> liftExp b
  OpLE a b -> OpLE <$> liftExp a <*> liftExp b

  OpGT a b -> OpGT <$> liftExp a <*> liftExp b
  OpGE a b -> OpGE <$> liftExp a <*> liftExp b


liftPat :: MonadLift m => Pat -> m Pat
liftPat = \case
  PVar v -> return $ PVar v
  PCon n ps ->
    PCon <$> pure n <*> mapM liftPat ps
  
  PTuple p ps ->
    PTuple <$> liftPat p <*> mapM liftPat ps

  PWild -> pure PWild

  PType p ty ->
    PType <$> liftPat p <*> liftType ty
  
  PLoc p l -> withLoc l (PLoc <$> liftPat p <*> pure l)
  PParens p -> PParens <$> liftPat p


liftClause :: MonadLift m => Clause -> m Clause
liftClause (Clause may_l bnd) = withMaybeLoc may_l $ do
  (p, e) <- unbind bnd
  p' <- liftPat p
  e' <- liftExp e
  return $ Clause may_l (bind p' e')