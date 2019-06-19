{-# LANGUAGE LambdaCase,
             FlexibleInstances, 
             MultiParamTypeClasses, 
             FlexibleContexts,
             DeriveGeneric,
             DeriveDataTypeable
  #-}
module Language.SystemF.Syntax where

import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Unbound.Generics.LocallyNameless

-- SystemF, The Polymorphic Lambda Calculus
--   Invariants: Type checked

type Var = Name Exp

data FuncClosure
  = FCEmpty
  | FCCons (Bind Func FuncClosure)
  deriving (Show, Generic, Typeable)
  

data Func = Func Type (Rebind Var (Bind [Pat] Exp))
  deriving (Show, Generic, Typeable)


data Exp
  = EVar Var
  | EType Exp Type
  | EApp Exp [Exp]
  | ELam (Bind [Pat] Exp)
  | ELet (Bind LetBind Exp)
  | ECase Exp [Clause]
  | ECon String [Exp]
  | EInt Int
  | ENewCon String [Exp]
  | EFree Exp
  deriving (Show, Generic, Typeable)
  

data LetBind 
  = LetEmpty
  | LetVars (Rebind (Pat, Embed Exp) LetBind)
  | LetFunc (Rebind (Var, Embed (Bind [Var] Exp)) LetBind)
  deriving (Show, Generic, Typeable)

data Clause = Clause (Bind Pat Exp)
  deriving (Show, Generic, Typeable)

data Pat
  = PVar Var
  | PCon String [Pat]
  | PWild
  | PType Pat Type
  deriving (Show, Generic, Typeable)


data Type
  = TVar String
  | TI32
  | TCon String
  | TArr Type Type
  deriving (Show, Generic, Typeable)