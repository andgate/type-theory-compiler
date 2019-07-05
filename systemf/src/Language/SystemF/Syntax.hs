{-# LANGUAGE LambdaCase,
             FlexibleInstances, 
             MultiParamTypeClasses, 
             FlexibleContexts,
             DeriveGeneric,
             DeriveDataTypeable
  #-}
module Language.SystemF.Syntax where

import Language.Syntax.Location

import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Unbound.Generics.LocallyNameless

-- SystemF, The Polymorphic Lambda Calculus
--   Invariants: Type checked

type Var = Name Exp

data Module = Module String [Func]

data Func = Func Type (Rebind Var (Bind [Pat] Exp))
  deriving (Show, Generic, Typeable)


data Exp
  = EVar Var
  | EApp Exp [Exp]
  | EType Exp Type
  | ECast Exp Type
  | ELoc Exp Loc
  | EParens Exp
  | ELam (Bind [Pat] Exp)
  | ELet (Bind LetBind Exp)
  | ECase Exp [Clause]
  | ECon String [Exp]
  | EInt Int
  | ENewCon String [Exp]
  | EFree Exp
  deriving (Show, Generic, Typeable)



-----------------------------------------------------------------
-- Literals
-----------------------------------------------------------------

-- Literals
data Lit
  = LNull
  | LInt Int
  | LDouble Double
  | LBool Bool
  | LChar Char
  | LString String
  | LStringI Int
  | LArray [Exp]
  | LArrayI Int
  | LGetI Exp Int
  deriving (Show, Generic, Typeable)


-----------------------------------------------------------------
-- Expression Extras
-----------------------------------------------------------------

data LetBind 
  = LetEmpty
  | LetVars (Rebind (Pat, Embed Exp) LetBind)
  | LetFunc (Rebind (Var, Embed (Bind [Var] Exp)) LetBind)
  deriving (Show, Generic, Typeable)

data Clause = Clause (Bind Pat Exp)
  deriving (Show, Generic, Typeable)

-----------------------------------------------------------------
-- Patterns
-----------------------------------------------------------------

data Pat
  = PVar Var
  | PCon String [Pat]
  | PWild
  | PType Pat Type
  deriving (Show, Generic, Typeable)

-----------------------------------------------------------------
-- Types
-----------------------------------------------------------------

type TVar = Name Type
data Type
  = TUnit
  | TVar TVar
  | TArr Type Type
  | TCon String
  | TI8
  | TI32
  | TChar
  | TArray Int Type 
  | TPtr Type
  | TString
  | TVoid
  deriving (Show, Generic, Typeable)


instance Alpha Type
instance Alpha Exp
instance Alpha Lit
instance Alpha Pat
instance Alpha Clause
instance Alpha LetBind
instance Alpha Loc
instance Alpha Region
instance Alpha Position

instance Subst Type Loc
instance Subst Type Region
instance Subst Type Position
instance Subst Type Clause
instance Subst Type LetBind
instance Subst Type Pat
instance Subst Type Lit
instance Subst Type Exp
instance Subst Type Type where
  isvar (TVar v) = Just (SubstName v)
  isvar _  = Nothing


instance Subst Exp Loc
instance Subst Exp Region
instance Subst Exp Position
instance Subst Exp Clause
instance Subst Exp LetBind
instance Subst Exp Pat
instance Subst Exp Lit
instance Subst Exp Type
instance Subst Exp Exp where
  isvar (EVar v) = Just (SubstName v)
  isvar _  = Nothing


newTVar :: Fresh m => m Type
newTVar =
  TVar <$> fresh (s2n "x")