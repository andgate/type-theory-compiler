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

data Func = Func String Type (Bind [Var] Exp)
  deriving (Show, Generic, Typeable)

data Exp
  = EVar Var
  | EApp Exp [Exp]
  | ELet (Bind (Maybe Pat, Embed Exp) Exp)
  | EType Exp Type
  | EInt Int
  | ECase Exp [(Pat, Exp)]
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


data Pat
  = PVar Var
  | PCon String [Pat]
  deriving (Show, Generic, Typeable)

data Clause = Clause String (Bind [Var] Exp) 

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

instance Subst Exp Func 
instance Subst Exp Type
instance Subst Exp Pat
instance Subst Exp Exp where
  isvar (EVar v) = Just (SubstName v)
  isvar _  = Nothing