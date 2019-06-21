{-# LANGUAGE LambdaCase,
             FlexibleInstances, 
             MultiParamTypeClasses, 
             FlexibleContexts,
             DeriveGeneric,
             DeriveDataTypeable,
             DataKinds,
             GADTs,
             KindSignatures,
             StandaloneDeriving
  #-}
module Language.STLC.Lifted.Syntax where

import Data.Bifunctor
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Unbound.Generics.LocallyNameless


-- The Simply Typed Lambda Calculus
--   Invariants: Fully type annotated
--             , Lambda Lifted
--             , Monomorphic
--             , No Partial Application

type Var = Name Exp

data Module = Module String [Defn]

data Defn
  = FuncDefn Func
  | ExternDefn Extern
  | DataTypeDefn DataType
  deriving (Show, Generic, Typeable)

data Extern = Extern String [Type] Type
  deriving (Show, Generic, Typeable)

data DataType =
  DataType String [(String, [(Maybe String, Type)])]
  deriving (Show, Generic, Typeable)

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

data Func = Func Type String (Bind [Pat] Exp)
  deriving (Show, Generic, Typeable)


func :: String -> Type -> [Pat] -> Exp -> Func
func n ty ps body = Func ty n (bind ps body)

---------------------------------------------------------------------------
-- Expressions
---------------------------------------------------------------------------

type Exp = Exp' Type
data Exp' t
  = EVar Var
  | ELit (Lit' (Exp' t))
  | EType (Exp' t) t
  | EApp (Exp' t) [Exp' t]
  | ELet (Bind (Rec [(Pat' t, Embed (Exp' t))]) (Exp' t))
  | EIf (Exp' t) (Exp' t) (Else' t)
  | ECase (Exp' t) [Clause' t]

  | EDeref (Exp' t)
  | ERef (Exp' t)
  
  | ECon String [Exp' t]
  | ENewCon String [Exp' t]
  | EFree (Exp' t)

  | EGet (Exp' t) String
  | EGetI (Exp' t) (Exp' t)
  | ESet (Exp' t) (Exp' t)

  | ENewArray [Exp' t]
  | ENewArrayI (Exp' t)
  | EFreeArray (Exp' t)
  | EResizeArray (Exp' t) (Exp' t)
  | EArrayElem (Exp' t) (Exp' t)

  | ENewString (Exp' t)
  | ENewStringI (Exp' t)

  | EOp (Op' t)
  deriving (Show, Generic, Typeable)

-- Literals
type Lit = Lit' Exp
type FullLit = Lit' LLit
data Lit' e
  = LI32 Int
  | LI8 Int
  | LChar Char
  | LString String
  | LStringI e
  | LArray [e]
  | LArrayI e
  deriving (Show, Generic, Typeable)

-- Else Branches
type Else = Else' Type
data Else' t
  = Else (Exp' t)
  | Elif (Exp' t) (Exp' t) (Else' t)
  deriving (Show, Generic, Typeable)

-- Operations
type Op = Op' Type
data Op' t
  = OpAddI (Exp' t) (Exp' t)
  | OpSubI (Exp' t) (Exp' t)
  | OpMulI (Exp' t) (Exp' t)
  | OpAddF (Exp' t) (Exp' t)
  | OpSubF (Exp' t) (Exp' t)
  | OpMulF (Exp' t) (Exp' t)
  deriving (Show, Generic, Typeable)


--Helpers

evar :: String -> Exp
evar = EVar . s2n

eapp :: String -> [Exp] -> Exp
eapp f_n xs = EApp (evar f_n) xs 

elet :: [(Pat, Exp)] -> Exp -> Exp
elet qs body = ELet (bind (rec (second embed <$> qs)) body)

ecase :: Exp -> [(Pat, Exp)] -> Exp
ecase e qs = ECase e [Clause (bind p e') | (p, e') <- qs]


---------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------


data TypeKind = Mono | Poly

type TVar = Name Type

type Type = Type' Mono -- STLC only allows mono types
data Type' :: TypeKind -> * where
  TUnit :: Type' a
  TVar :: TVar -> Type' Poly
  TArr :: Type' a -> Type' a -> Type' a
  TCon :: String -> Type' a
  TI8 :: Type' a
  TI32 :: Type' a
  TChar :: Type' a
  TArray Int Type :: Int -> Type' a -> Type' a 
  TPtr :: Type' a -> Type' a
  TString :: Type' a
  TVoid :: Type' a
  deriving (Show, Generic, Typeable)


-- Type Equality

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


-- Helpers

tarr :: [Type] -> Type -> Type
tarr paramtys retty
  = foldr (\a b -> TArr a b) retty paramtys

exType :: Exp -> Type
exType (EType _ ty) = ty
exType _ = error "Expected typed expression!"


---------------------------------------------------------------------------
-- Patterns
---------------------------------------------------------------------------

data Pat
  = PVar Var
  | PCon String [Pat]
  | PWild
  | PType Pat Type
  deriving (Show, Generic, Typeable)

-- Clauses (Case branches)
data Clause = Clause (Bind Pat Exp)
  deriving (Show, Generic, Typeable)

-- Helpers

pvar :: String -> Pat
pvar = PVar . s2n

patTypedVars :: Pat -> [(String, Type)]
patTypedVars (PType p ty) = patTypedVars' ty p

patTypedVars' :: Type -> Pat -> [(String, Type)]
patTypedVars' ty = \case
  PVar v -> [(name2String v, ty)]
  PCon n ps -> concatMap patTypedVars ps
  PWild -> []
  PType p ty' -> patTypedVars' ty' p


splitType :: Type -> ([Type], Type)
splitType = \case
  TArr t1 t2 ->
    let (ts, t2') = splitType t2
    in (t1:ts, t2')
  t -> ([], t)


---------------------------------------------------------------------------
-- Alpha and Subst instances for unbound-generics
---------------------------------------------------------------------------

instance Alpha Exp
instance Alpha Type
instance Alpha Func
instance Alpha Pat
instance Alpha Clause
instance Alpha Op
instance Alpha a => Alpha (Telescope a)

instance Subst Exp Func 
instance Subst Exp Type
instance Subst Exp Pat
instance Subst Exp Clause
instance Subst Exp Op
instance Subst Exp a => Subst Exp (Telescope a)
instance Subst Exp Exp where
  isvar (EVar v) = Just (SubstName v)
  isvar _  = Nothing

instance Subst Type Func
instance Subst Type Pat
instance Subst Type Clause
instance Subst Type Op
instance Subst Type a => Subst Type (Telescope a)
instance Subst Type Exp where
instance Subst Type Type where
  isvar (TVar v) = Just (SubstName v)
  isvar _  = Nothing