{-# LANGUAGE LambdaCase #-}
module Language.ANF.Syntax where

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
  = ECall String [Val]
  | EVal Val
  | ELet (Maybe String) Type Exp Type Exp
  | EIf Val Exp Else
  | EMatchI Val Type [(Int, Exp)]
  | EMatch Val Type [(String, [Maybe String], Exp)]
  | EOp Op

data Else
  = Else Exp
  | Elif Val Exp Else

data Val
  = VVar String
  | VInt Int
  | VString String
  | VArray [Val]

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
  = AddOpI Val Val
  | SubOpI Val Val
  | MulOpI Val Val

data FArithOp
  = AddOpF Val Val
  | SubOpF Val Val
  | MulOpF Val Val

data Constr = Constr String [Val]

data MemOp
  = ConstrOp Constr    -- Stack allocation
  | NewOp Constr       -- Heap allocation
  | FreeOp Val         -- Heap deallocation
  | MemAccessI Val Val
  | MemAccess Val String  -- Memory access, can either be an integer or some member string
  | MemUpdate String Val  -- Memory update

data PtrOp
  = RefOp Type Val
  | DerefOp Type Val

data ArrayOp
  = NewArrayOp Int Type
  | FreeArrayOp Val
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