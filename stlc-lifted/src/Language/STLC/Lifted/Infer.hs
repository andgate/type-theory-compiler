{-# LANGUAGE LambdaCase
           , ConstraintKinds
           , FlexibleContexts
          #-}
module Language.STLC.Lifted.Infer where

-- Type Inference
--   Type inference will enrich the ast with type annotations.
-- While this language is intended to be the target of a
-- higher level language, type inference is necessary
-- for generating tests.

import Language.STLC.Lifted.Syntax

import Control.Monad.Reader

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Unbound.Generics.LocallyNameless


type Env = Map Var Type
type Infer = ReaderT Env FreshM
type MonadInfer m = (MonadReader Env m, Fresh m)

lookupType :: MonadReader Env m => Var -> m Type
lookupType n
  = reader (Map.lookup n) >>= maybe err return
  where err = error $ "Check: untyped variable encounted: " ++ name2String n

withType :: MonadReader Env m => Var -> Type -> m a -> m a
withType n ty
  = local (Map.insert n ty)

withTypes :: MonadReader Env m => [(Var, Type)] -> m a -> m a
withTypes ns = local (\env -> foldl f env ns)
  where f env (n, ty) = Map.insert n ty env 

infer :: MonadInfer m => Exp -> m Exp
infer = \case
  e@(EVar v) -> EType e <$> lookupType v

  EType e t -> do
    e' <- infer e
    case e' of
      EType _ t'
        | t /= t' -> error "Type mismatch!"
        | True    -> return e

      _ -> error "Type inference has mysteriously failed!"

  EApp f xs -> do
    f' <- infer f
    xs' <- mapM infer xs
    let f_ty = exType f'
        x_tys = exType <$> xs'
        ty = foldr TArr f_ty x_tys
    return $ EType (EApp f' xs') ty

  ELet bnd -> do
    (letbnds, body) <- unbind bnd
    let (ps, es) = unzip $ untelescope letbnds
    ps' <- mapM inferPat ps
    let ns = concatMap patTypedVars ps'
    withTypes ns $ do
      es' <- mapM (infer . unembed) es
      body' <- infer body
      let letbnds' = telescope (zip ps' (embed <$> es'))
      return $ EType (ELet $ bind letbnds' body')
                     (exType body')

  ECon n args -> do
    ty <- (snd . splitType) <$> lookupType (s2n n)
    e' <- ECon n <$> mapM infer args
    return $ EType e' ty

  e@(EInt _) -> return $ EType e TI32

  ENewCon n args -> do
    ty <- (snd . splitType) <$> lookupType (s2n n)
    e' <- ENewCon n <$> mapM infer args
    return $ EType e' ty

  EFree e ->
    EType <$> (EFree <$> infer e) <*> pure TVoid


inferPat :: MonadInfer m => Pat -> m Pat
inferPat = \case
  PVar v -> return $ PVar v

  PCon n ps -> do
    ty <- lookupType (s2n n)
    let retty = snd $ splitType ty 
        argtys = fst $ splitType ty 
    ps' <- mapM (\(p, t) -> inferPat (PType p t)) (zip ps argtys)
    return $ PType (PCon n ps') retty

  PWild -> return PWild

  PType p t -> do
    p' <- inferPat p
    case p' of
      PType _ t' ->
        if t /= t'
          then error $ "Patter type mismatch"
          else return p'
      _ -> error "Inference on patterns has mysteriously failed!"

