{-# LANGUAGE LambdaCase, FlexibleContexts, RankNTypes #-}
module Eval where

import Syntax

import Unbound.Generics.LocallyNameless

import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)


type Eval = FreshMT (Reader Env)
data Env = Env { envTerms :: Map Text Value }

insertEnv :: Text -> Value -> Env -> Env
insertEnv n v (Env ts) = Env (Map.insert n v ts)

runEval :: Eval a -> Env ->  a
runEval m env = runReader (runFreshMT m) env

lookupEnv :: MonadReader Env m => Text -> m (Maybe Value)
lookupEnv n = (Map.lookup n . envTerms) <$> ask

localEnv :: MonadReader Env m => Text -> Value -> m a -> m a
localEnv n v m = local (insertEnv n v) m

eval :: Term -> Eval Value
eval = \case
  TType -> return VType
  TUnit -> return VUnit
  TAnnot t _ -> eval t

  TVar n -> do
    may_v <- lookupEnv (name2Text n) 
    case may_v of
      Nothing -> error "Name not found"
      Just v -> return v
  
  TPi _  bnd -> return $ VPi bnd
  TLam _ bnd -> return $ VLam bnd

  TApp f x -> do
    fv <- eval f
    xv <- eval x
    case fv of
      VLam bnd -> do
        ((v, _), body) <- unbind bnd
        eval (subst v x body)
      _ -> error "Can't apply a non-function"

  TProd _ bnd -> return $ VProd bnd
  TPair a b ->
     VPair <$> eval a <*> eval b

  TFst p -> do
    v <- eval p
    case v of
      VPair a b -> return a
      _ -> error "Cannot call first on non-pair"

  TSnd p -> do
    v <- eval p
    case v of
      VPair _ b -> return b
      _ -> error "Cannot call second on non-pair"
  

  TLet bnd -> do
    ((v1, v2, Embed rhs), body) <- unbind bnd
    rhs' <- eval rhs
    case rhs' of
      VPair a b -> localEnv (name2Text v1) a
                     $ localEnv (name2Text v2) b
                     $ eval body
      _ -> error "Expected a pair!"


  TSeq t1 t2 ->
    let t1' = eval t1 in t1' `seq` eval t2

  TBool -> return VType
  TTrue  -> return $ VBool True
  TFalse -> return $ VBool False
  TIf pt elimT elimF -> do
    pv <- eval pt
    case pv of
      VBool p -> eval (if p then elimT else elimF)
      _ -> error "Expected a boolean value as the predicate"

