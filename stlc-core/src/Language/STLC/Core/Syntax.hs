{-# LANGUAGE LambdaCase #-}
module Language.STLC.Core.Syntax where

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

data Defn
  = FuncDefn Func
  | ExternDefn Extern
  | DataTypeDefn DataType

data Func = Func String [(String, Type)] Exp Type

data Extern = Extern String [Type] Type

data DataType =
  DataType String [(String, [(Maybe String, Type)])]

data Exp
  = ECall Exp [Exp]
  | EVal Val
  | ELet [(Pat, Exp)] Exp
  | EIf Exp Exp Else
  | EMatchI Exp [(Int, Exp)]
  | EMatch Exp (NonEmpty (String, [Maybe String], Exp))
  | EOp Op
  | EType Exp Type

data Pat
  = PVar String
  | PCon String [Pat]
  | PWild
  | PType Type

data Else
  = Else Exp
  | Elif Exp Exp Else

data Val
  = VVar String
  | VInt Int
  | VString String
  | VArray [Exp]

data Type
  = TCon String
  | TI8
  | TI32
  | TArray Int Type
  | TPtr Type
  | TString
  | TVoid
  | TFunc Type [Type]

data Op
  = IArithOp IArithOp
  | FArithOp FArithOp
  | PtrOp PtrOp
  | MemOp MemOp
  | ArrayOp ArrayOp

data IArithOp
  = AddOpI Exp Exp
  | SubOpI Exp Exp
  | MulOpI Exp Exp

data FArithOp
  = AddOpF Exp Exp
  | SubOpF Exp Exp
  | MulOpF Exp Exp

data Constr = Constr String [Exp]

data MemOp
  = ConstrOp Constr    -- Stack allocation
  | NewOp Constr       -- Heap allocation
  | FreeOp Exp         -- Heap deallocation
  | MemAccessI Exp Exp
  | MemAccess Exp String  -- Memory access, can either be an integer or some member string
  | MemUpdate String Exp  -- Memory update

data PtrOp
  = RefOp Exp
  | DerefOp Exp

data ArrayOp
  = NewArrayOp Int Type
  | FreeArrayOp Exp
  | ResizeArrayOp String Int Type
  | AccessArrayOp String Int

data StringOp
  = StrConstr String


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