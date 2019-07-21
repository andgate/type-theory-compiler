{-# LANGUAGE ConstraintKinds
           , LambdaCase
           , FlexibleContexts
           , GeneralizedNewtypeDeriving
          #-}
module Language.STLC.Namecheck where


import Language.Syntax.Location
import Language.STLC.Syntax

import Control.Monad.Reader
import Control.Monad.Report
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Unbound.Generics.LocallyNameless

--- Namecheck Environment
data Env = Env
  { envNames :: Map String Loc
  , envCons :: Map String Loc
  , envTyCons :: Map String Loc
  , envLoc :: Loc  
  }

mkEnv :: Map String Loc -> Map String Loc -> Map String Loc -> Loc -> Env
mkEnv vars cons tycons l
  = Env { envNames = vars
        , envCons = cons
        , envTyCons = tycons
        , envLoc = l }

envInsertNames :: [(String, Loc)] -> Env -> Env
envInsertNames ns env = env { envNames = Map.union (Map.fromList ns) (envNames env) }

--- Namecheck Errors

data Err = NcErr String

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

checkName  :: MonadNamecheck m => String -> m ()
checkName _ = undefined

checkCon   :: MonadNamecheck m => String -> m ()
checkCon _ = undefined

checkTyCon :: MonadNamecheck m => String -> m ()
checkTyCon _ = undefined


-- Namecheck requires some global name sets and a module to check on
namecheck :: Map String Loc -> Map String Loc -> Map String Loc
          -> Module -> Either [Err] Module
namecheck vars cons tycons m
  = runNamecheck (namecheckModule m)
                 (mkEnv vars cons tycons (locOf m))


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
namecheckDataType _ = undefined


namecheckType :: MonadNamecheck m => Type -> m Type
namecheckType = \case
  TArr t1 t2 -> TArr <$> namecheckType t1 <*> namecheckType t2
  TCon n -> do
    checkTyCon n
    return $ TCon n
  
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
  _ -> undefined

namecheckPat :: MonadNamecheck m => Pat -> m Pat
namecheckPat = \case
  PVar v -> return $ PVar v
  PCon n ps -> do
    checkName n
    PCon n <$> mapM namecheckPat ps
  
  PTuple p ps ->
    PTuple <$> namecheckPat p <*> mapM namecheckPat ps

  PWild -> pure PWild

  PType p ty ->
    PType <$> namecheckPat p <*> namecheckType ty
  
  PLoc p l -> withLoc l (PLoc <$> namecheckPat p <*> pure l)
  PParens p -> PParens <$> namecheckPat p