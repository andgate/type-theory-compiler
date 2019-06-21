{-# LANGUAGE LambdaCase #-}
module Language.LLTT.Syntax where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- A-Normal Form Simply Typed Lambda Calculus
--   Invariants: Fully Type Annotated
--             , Lambda Lifted
--             , Monomorphic
--             , No Partial Application
--             , No Nested Applications
--             , Single-level Pattern matching

---------------------------------------------------------------------------
-- Module and Definitions
---------------------------------------------------------------------------

data Module = Module String [Defn]

data Defn
  = FuncDefn Func
  | ExternDefn Extern
  | DataTypeDefn DataType
  deriving(Show)

data Func = Func String [Pat] Exp
  deriving(Show)

data Extern = Extern String [Type] Type
  deriving(Show)

---------------------------------------------------------------------------
-- Data Types
---------------------------------------------------------------------------

data DataType =
  DataType String [(String, [(Maybe String, Type)])]
  deriving(Show)

sizeDataType :: Map String Int -> DataType -> Int
sizeDataType sizes (DataType _ cs) =
  maximum [sum [sizeType sizes ty | (_, ty) <- ps] | (_, ps) <- cs]

sizeType :: Map String Int -> Type -> Int
sizeType sizes = \case
  TCon n -> case Map.lookup n sizes of
    Nothing -> error "DataType not registered"
    Just i -> i
  TI8 -> 1
  TI32 -> 4
  TArray n ty -> n + sizeType sizes ty
  TPtr _ -> 8
  TString -> 8
  TVoid -> 1


---------------------------------------------------------------------------
-- Expressions
---------------------------------------------------------------------------

data Exp
  = EVar String
  | ELit Lit
  | ECall Exp (NonEmpty Exp)
  | EType Exp Type
  | ELet (NonEmpty (Pat, Exp)) Exp
  | EIf Exp Exp Else
  | EMatchI Exp (NonEmpty (Int, Exp))  -- ^ This will disappear
  | EMatch Exp (NonEmpty Clause)

  | ERef Exp
  | EDeref Exp

  | ECon String [Exp]
  | ENewCon String [Exp]
  | EFree Exp

  | EGet Exp String
  | EGetI Exp Int
  | ESet Exp Exp

  | ENewArray [Exp]
  | ENewArrayI Exp
  | EResizeArray Exp Exp
  | EArrayElem Exp Exp

  | ENewString String
  | ENewStringI Exp

  | EOp Op
  deriving(Show)

-- Literals
data Lit
  = LI32 Int
  | LI8 Int
  | LChar Char
  | LString String
  | LStringI Int
  | LArray [Exp]
  | LArrayI Int
  deriving(Show)

  -- If Branches
data Else
  = Else Exp
  | Elif Exp Exp Else
  deriving(Show)

-- Operations
data Op
  = OpAddI Exp Exp
  | OpSubI Exp Exp
  | OpMulI Exp Exp
  | OpAddF Exp Exp
  | OpSubF Exp Exp
  | OpMulF Exp Exp
  deriving(Show)


---------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------

data Type
  = TCon String
  | TI8
  | TI32
  | TChar
  | TArray Int Type
  | TPtr Type
  | TString
  | TVoid
  | TFunc Type (NonEmpty Type)
  deriving(Show)


---------------------------------------------------------------------------
-- Patterns
---------------------------------------------------------------------------

data Pat
  = PVar String
  | PCon String [Pat]
  | PWild
  | PType Pat Type
  deriving(Show)

-- Case branches
data Clause = Clause String [Maybe String] Exp
  deriving(Show)