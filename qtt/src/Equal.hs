{-# LANGUAGE LambdaCase, ViewPatterns #-}
module Equal where

import Syntax
import Check.Monad

import Unbound.Generics.LocallyNameless

equate :: Term -> Term -> Tc ()
equate t1 t2 = if (aeq t1 t2) then return () else do
  n1 <- whnf' False t1  
  n2 <- whnf' False t2
  case (n1, n2) of
    (TType, TType) -> return ()
    
    (TVar x,  TVar y)  | x == y -> return ()
    
    (TLam r1 bnd1, TLam r2 bnd2) -> do
      Just (_, b1, _, b2) <- unbind2 bnd1 bnd2
      equate b1 b2
    
    (TApp a1 a2, TApp b1 b2) -> do
      equate a1 b1 
      equate a2 b2
      
    (TPi r1 bnd1, TPi r2 bnd2) -> do
      Just ((_, unembed -> tyA1), tyB1, 
           (_, unembed -> tyA2), tyB2) <- unbind2 bnd1 bnd2
      equate tyA1 tyA2                                             
      equate tyB1 tyB2

  -- Missing some cases...

    _ -> error $ "Type mismatch\nT1: " ++ show n1 ++ "\nT2: " ++ show n2


-- | Ensure that the given type 'ty' is a 'Pi' type
-- (or could be normalized to be such) and return the components of 
-- the type.
-- Throws an error if this is not the case.
ensurePi :: Type -> Tc (TName, Type, Type)
ensurePi ty = do
  nf <- whnf ty
  case nf of 
    (TPi _ bnd) -> do 
      ((x, unembed -> tyA), tyB) <- unbind bnd
      return (x, tyA, tyB)

    _ -> error $ "Expected a function type, instead found " ++ show nf


whnf :: Term -> Tc Term
whnf = whnf' False

whnf' :: Bool -> Term -> Tc Term
whnf' b = \case
  TVar n -> do
    defn_may <- lookupDef n
    case defn_may of
      Just d -> whnf' b d
      Nothing
        | b    -> error "records not implemented, can't lookup record"
        | True -> return (TVar n) 

  TApp t1 t2 -> do
    nf <- whnf' b t1
    case nf of
      TLam _ bnd -> do
        ((x, _), body) <- unbind bnd
        whnf' b (subst x t2 body)
      
      TVar y -> do
        nf2 <- whnf' b t2
        maybeDef <- lookupDef y -- should be lookupRecDef
        case maybeDef of
          Just d -> whnf' False $ TApp d nf2
          _ -> return $ TApp nf nf2
      
      _ -> return $ TApp nf t2

  -- Missing some cases...

  tm -> return tm