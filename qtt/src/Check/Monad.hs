{-# LANGUAGE FlexibleContexts #-}
module Check.Monad where

import Syntax

import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack, unpack)

import Unbound.Generics.LocallyNameless

type Tc = FreshMT (Reader Env)

data Env = Env { envDefs :: Map Text Term
               , envSigs :: Map Text Type }

runTc :: Env -> Tc a -> a
runTc env m = runReader (runFreshMT m) env

insertEnvSig :: Text -> Type -> Env -> Env
insertEnvSig n ty (Env defs sigs) = Env defs (Map.insert n ty sigs) 

lookupTyMaybe :: MonadReader Env m => TName -> m (Maybe Term)
lookupTyMaybe n = do
  env <- asks envSigs
  return $ Map.lookup (name2Text n) env

lookupTy :: MonadReader Env m => TName -> m Term
lookupTy n = do
  ty_may <- lookupTyMaybe n
  case ty_may of
    Just ty -> return ty
    Nothing -> error "Unexpected name encountered"


lookupDef :: MonadReader Env m => TName -> m (Maybe Term)
lookupDef n =
  Map.lookup (name2Text n) <$> asks envDefs

extendEnvSig :: MonadReader Env m => Text -> Type -> m a -> m a
extendEnvSig n ty m = local (insertEnvSig n ty) m