{-# LANGUAGE LambdaCase,
             FlexibleInstances, 
             MultiParamTypeClasses, 
             FlexibleContexts,
             DeriveGeneric,
             DeriveDataTypeable
  #-}
module Language.STLC.Lifted.Syntax where

import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Unbound.Generics.LocallyNameless


-- The Simply Typed Lambda Calculus
--   Invariants: Fully type annotated
--             , Lambda Lifted
--             , Monomorphic
--             , No Partial Application

type Var = Name Exp

data FClosure
  = FCEmpty
  | FCCons (Bind Func FClosure)
  deriving (Show, Generic, Typeable)

type Funcs = Telescope Func

data Telescope a
  = TeleEmpty
  | TeleCons (Rebind a (Telescope a))
  deriving (Show, Generic, Typeable)

telescope :: Alpha a => [a] -> Telescope a
telescope [] = TeleEmpty
telescope (a:as) = TeleCons (rebind a (telescope as))

untelescope :: Alpha a => Telescope a -> [a]
untelescope TeleEmpty = []
untelescope (TeleCons bnd) =
  let (a, t) = unrebind bnd
  in a : untelescope t

data Func = Func Type (Rebind Var (Bind [Pat] Exp))
  deriving (Show, Generic, Typeable)

data Exp
  = EVar Var
  | EType Exp Type
  | EApp Exp [Exp]
  | ELet (Bind (Telescope (Pat, Embed Exp)) Exp)
  | ECase Exp [Clause]
  | ECon String [Exp]
  | EInt Int
  | ENewCon String [Exp]
  | EFree Exp
  deriving (Show, Generic, Typeable)


data Type
  = TArr Type Type 
  | TCon String
  | TI8
  | TI32
  | TArray Int Type
  | TPtr Type
  | TString
  | TVoid
  deriving (Show, Generic, Typeable)


instance Eq Type where
  (==) a b = case (a, b) of
    (TArr a1 a2, TArr b1 b2) -> (a1 == b1) && (a2 == b2)
    (TCon n1, TCon n2) -> n1 == n2
    
    (TI8, TI8)   -> True
    (TI32, TI32) -> True

    (TArray i1 t1, TArray i2 t2) -> (i1 == i2) && (t1 == t2) 
    (TPtr t1, TPtr t2) -> t1 == t2
    (TString, TString) -> True
    (TVoid, TVoid)     -> True

    _ -> False


exType :: Exp -> Type
exType (EType _ ty) = ty
exType _ = error "Expected typed expression!"


data Pat
  = PVar Var
  | PCon String [Pat]
  | PWild
  | PType Pat Type
  deriving (Show, Generic, Typeable)

data Clause = Clause String (Bind [Var] Exp)
  deriving (Show, Generic, Typeable)


patTypedVars :: Pat -> [(Var, Type)]
patTypedVars (PType p ty) = patTypedVars' ty p

patTypedVars' :: Type -> Pat -> [(Var, Type)]
patTypedVars' ty = \case
  PVar v -> [(v, ty)]
  PCon n ps -> concatMap patTypedVars ps
  PWild -> []
  PType p ty' -> patTypedVars' ty' p


splitType :: Type -> ([Type], Type)
splitType = \case
  TArr t1 t2 ->
    let (ts, t2') = splitType t2
    in (t1:ts, t2')
  t -> ([], t)


instance Alpha Exp
instance Alpha Type
instance Alpha Func
instance Alpha Pat
instance Alpha Clause
instance Alpha a => Alpha (Telescope a)

instance Subst Exp Func 
instance Subst Exp Type
instance Subst Exp Pat
instance Subst Exp Clause
instance Subst Exp a => Subst Exp (Telescope a)
instance Subst Exp Exp where
  isvar (EVar v) = Just (SubstName v)
  isvar _  = Nothing


-------------------------------------------
-- Type Equality

