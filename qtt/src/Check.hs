{-# LANGUAGE LambdaCase, ViewPatterns, FlexibleContexts #-}
module Check where

import Syntax
import Equal
import Check.Monad

import Unbound.Generics.LocallyNameless


-- | Infer the type of a term, producing an annotated version of the 
-- term (whose type can *always* be inferred).
inferType :: Term -> Tc (Term,Type)
inferType t = tcTerm (t, Nothing)

checkType :: Term -> Type -> Tc (Term, Type)
checkType tm tyx = do
  nf <- whnf tyx
  tcTerm (tm, Just nf)


-- | Make sure that the term is a type (i.e. has type 'Type') 
tcType :: Term -> Tc Term
tcType t = fst <$> checkType t TType


tcTerm :: (Term, Maybe Type) -> Tc (Term, Type)
tcTerm = \case
  (t@TType, Nothing) -> return (t, TType)
  (t@TUnit, Nothing) -> return (t, TType)
  
  (t@(TVar n), Nothing) -> do
    ty <- lookupTy n
    return (t, ty)
  
  (TPi r bnd, Nothing) -> do
    ((n, unembed -> t1), t2) <- unbind bnd
    t1' <- tcType t1
    t2' <- extendEnvSig (name2Text n) t1' $ tcType t2
    return (TPi r (bind (n, embed t1') t2'), TType)

  (TLam r1 bnd1, Just (TPi r2 bnd2)) -> do
    Just ((x, unembed -> mt), body, (y, unembed -> t1), t2) <- unbind2 bnd1 bnd2
    maybe (return ()) (equate t1) mt
    (body', t2') <- extendEnvSig (name2Text x) t1 $ checkType body t2
    return (TLam r1 (bind (x, embed mt) body'), TPi r2 (bind (y, embed t1) t2'))
        
  (TLam _ _, Just _) -> error "Lambda expression without function type encountered"

  (TLam r bnd, Nothing) -> do
    ((x, unembed -> mt), body) <- unbind bnd
    t1 <- maybe (error "Must annotate lambda") return mt
      -- Check type annotation is well-formed
    t1' <- tcType t1
    -- Infer the type of the body of the lambda expression
    (body', t2') <- extendEnvSig (name2Text x) t1' $ inferType body
    return (TLam r (bind (x, embed $ Just t1') body'),
            TPi r (bind (x, embed t1') t2'))

  (TApp t1 t2, Nothing) -> do
    (t1', ty1)    <- inferType t1
    (x, tyA, tyB) <- ensurePi ty1
    (t2', ty2)    <- checkType t2 tyA
    return (TApp t1' t2', subst x t2' tyB)

  (TUnit, Nothing) -> return (TUnit, TType)

  (tm, Just ty) -> do
    (tm', ty') <- inferType tm
    equate ty ty'
    return (tm', ty)

  _ -> error "Unfinished type checking behaviour encountered!"

inferTerm :: Term -> Tc Term
inferTerm = \case
  TVar n -> undefined
  _ -> error "Unfinished type inference"


checkUsage :: (String -> Usage) -> Type