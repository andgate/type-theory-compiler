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

data Func = Func Type (Bind [Var] Exp)

data Exp
  = EVar Var
  | EApp Exp Exp

data Type
  = TVar String
  | TI32
  | TCon String
  | TArr Type Type