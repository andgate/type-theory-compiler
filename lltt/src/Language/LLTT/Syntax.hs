{-# LANGUAGE LambdaCase, ViewPatterns #-}
module Language.LLTT.Syntax where

import Language.Syntax.Location

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

data Module = Module Loc String [Defn]

data Defn
  = FuncDefn Func
  | ExternDefn Extern
  | DataTypeDefn DataType
  deriving(Show)

data Func = Func Loc String [Pat] Exp
  deriving(Show)

data Extern = Extern Loc String [Type] Type
  deriving(Show)

---------------------------------------------------------------------------
-- Data Types
---------------------------------------------------------------------------

data DataType =
  DataType Loc String [ConstrDefn]
  deriving(Show)

data ConstrDefn
  = ConstrDefn Loc String [Type]
  | RecordDefn Loc String (NonEmpty Entry)
  deriving (Show)

constrName :: ConstrDefn -> String
constrName = \case
  ConstrDefn _ n _ -> n
  RecordDefn _ n _ -> n

data Entry = Entry Loc String Type
  deriving (Show)


---------------------------------------------------------------------------
-- Data Type Size
---------------------------------------------------------------------------

sizeDataType :: Map String Int -> DataType -> Int
sizeDataType sizes (DataType _ _ cs)
  = maximum $ sizeConstrDefn sizes <$> cs

sizeConstrDefn :: Map String Int -> ConstrDefn -> Int
sizeConstrDefn sizes = \case
  ConstrDefn _ _ tys -> sum $ sizeType sizes <$> tys 
  RecordDefn _ _ ens -> sum $ sizeEntry sizes <$> ens

sizeEntry :: Map String Int -> Entry -> Int
sizeEntry sizes (Entry _ _ ty) = sizeType sizes ty

sizeType :: Map String Int -> Type -> Int
sizeType sizes = \case
  TVar _ -> 8 -- size of pointer, since its boxed. Might change later
  TCon n -> case Map.lookup n sizes of
    Nothing -> error "DataType not registered"
    Just i -> i
  TInt  s -> ceiling $ (fromIntegral s) / (8.0 :: Double)
  TUInt s -> ceiling $ (fromIntegral s) / (8.0 :: Double)
  TFp   s -> ceiling $ (fromIntegral s) / (8.0 :: Double) 
  TTuple t1 ts -> sizeType sizes t1 + foldl (\s t -> sizeType sizes t + s) (0 :: Int) ts
  TArray n ty -> n * sizeType sizes ty
  TVect  n ty -> n * sizeType sizes ty
  TPtr _ -> 8 -- asssume 64-bit system
  TFunc _ _ -> 8 -- size of pointer 
  TLoc t _ -> sizeType sizes t
  TParens t -> sizeType sizes t


---------------------------------------------------------------------------
-- Expressions
---------------------------------------------------------------------------

data Exp
  = EVar String
  | ELit Lit
  | ECall Exp (NonEmpty Exp)
  
  | EType Exp Type
  | ECast Exp Type
  
  | ELoc Exp Loc
  | EParens Exp

  | ELet (NonEmpty (Pat, Exp)) Exp
  | EIf Exp Exp Else
  | EMatch Exp (NonEmpty Clause)

  | ERef Exp
  | EDeref Exp

  | ETuple Exp (NonEmpty Exp)
  | ECon String [Exp]
  | ENewTuple Exp (NonEmpty Exp)
  | ENewCon String [Exp]
  | EFree Exp

  | EGet Exp String
  | EGetI Exp Exp
  | ESet Exp Exp

  | ENewArray [Exp]
  | ENewArrayI Exp
  | EResizeArray Exp Exp

  | ENewVect [Exp]
  | ENewVectI Exp

  | ENewString String

  | EOp Op
  deriving(Show)


exEAnn :: Exp -> Exp
exEAnn = \case
  ELoc e _ -> exEAnn e
  EType e _ -> exEAnn e
  EParens e -> exEAnn e
  e -> e 

exType :: Exp -> Type
exType = \case 
  EType _ ty -> ty
  ECast _ ty -> ty
  ELoc e _ -> exType e
  EParens e -> exType e
  e -> error $ "Expected typed expression, found: " ++ show e 

-- Literals
data Lit
  = LNull
  | LInt Integer
  | LDouble Double
  | LBool Bool
  | LChar Char
  | LString String
  | LArray [Exp]
  | LArrayI Int
  | LVect [Exp]
  | LVectI Int
  | LGetI Exp Int
  deriving(Show)

  -- If Branches
data Else
  = Else (Maybe Loc) Exp
  | Elif (Maybe Loc) Exp Exp Else
  deriving(Show)

-- Operations
data Op
  = OpAdd Exp Exp
  | OpSub Exp Exp
  | OpMul Exp Exp
  | OpDiv Exp Exp
  | OpRem Exp Exp
  | OpNeg Exp

  | OpAnd Exp Exp
  | OpOr  Exp Exp
  | OpXor Exp Exp
  | OpShR Exp Exp
  | OpShL Exp Exp

  | OpEq Exp Exp
  | OpNeq Exp Exp
  | OpLT Exp Exp
  | OpLE Exp Exp
  | OpGT Exp Exp
  | OpGE Exp Exp
  deriving (Show)


---------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------

data Type
  = TVar String
  | TCon String
  | TInt Int
  | TUInt Int
  | TFp Int
  | TTuple Type (NonEmpty Type)
  | TArray Int Type
  | TVect Int Type
  | TPtr Type
  | TFunc Type (NonEmpty Type)
  | TLoc Type Loc
  | TParens Type
  deriving(Show)


exTyAnn :: Type -> Type
exTyAnn = \case
  TLoc t _ -> exTyAnn t
  TParens t -> exTyAnn t
  t -> t

intSizes :: [Int]
intSizes = [1, 8, 16, 32, 64]

uintSizes :: [Int]
uintSizes = [8, 16, 32, 64]

floatSizes :: [Int]
floatSizes = [16, 32, 64, 128]

intTypes :: [Type]
intTypes = TInt <$> intSizes

uintTypes :: [Type]
uintTypes = TUInt <$> uintSizes

floatTypes :: [Type]
floatTypes = TFp <$> floatSizes

numTypes :: [Type]
numTypes = intTypes <> uintTypes <> floatTypes

isIntTy :: Type -> Bool
isIntTy (exTyAnn -> TInt _) = True
isIntTy _ = False

isUIntTy :: Type -> Bool
isUIntTy (exTyAnn -> TUInt _) = True
isUIntTy _ = False

isFloatTy :: Type -> Bool
isFloatTy (exTyAnn -> TFp _) = True
isFloatTy _ = False

isPtrTy :: Type -> Bool
isPtrTy (exTyAnn -> ty) = 
  case ty of
    TArray _ _ -> True
    TVect _ _ -> True
    TPtr _ -> True
    TFunc _ _ -> True
    _ -> False


---------------------------------------------------------------------------
-- Patterns
---------------------------------------------------------------------------

data Pat
  = PVar String
  | PCon String [Pat]
  | PTuple Pat (NonEmpty Pat)
  | PWild
  | PType Pat Type
  | PLoc Pat Loc
  | PParens Pat
  deriving(Show)

-- Case branches
data Clause = Clause String [Maybe String] Exp
  deriving(Show)

exPType :: Pat -> Type
exPType = \case
  PType _ ty -> ty
  PLoc p _ -> exPType p
  PParens p -> exPType p
  p -> error $ "error: Cannot extract Type from: " ++ show p

patFreeTyped :: Pat -> [(String, Type)]
patFreeTyped p = patFreeTyped' (exPType p) p

patFreeTyped' :: Type -> Pat -> [(String, Type)]
patFreeTyped' ty = \case
  PVar n -> [(n, ty)]
  PCon _ ps -> concatMap patFreeTyped ps
  PTuple p (NE.toList -> ps) ->
    concatMap patFreeTyped (p:ps)
  PWild -> []
  PType p ty' -> patFreeTyped' ty' p
  PLoc p _ -> patFreeTyped' ty p
  PParens p -> patFreeTyped' ty p


---------------------------------------------------------------------------
-- Location instances
---------------------------------------------------------------------------

instance HasLocation Module where
  locOf (Module l _ _) = l

instance HasLocation Exp where
  locOf = \case
    ELoc _ l -> l
    EType e _ -> locOf e
    EParens e -> locOf e
    _ -> error $ "expected located expression!"


instance HasLocation Type where
  locOf = \case
    TLoc _ l -> l
    TParens t -> locOf t
    _ -> error $ "expected located type!"

instance HasLocation Pat where
  locOf = \case
    PLoc _ l -> l
    PType p _ -> locOf p
    PParens p -> locOf p
    _ -> error $ "expected located pattern!"
