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
module Language.STLC.Syntax where

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

data Exp
  = EVar Var
  | ELit Lit
  | EType Exp Type
  | EApp Exp [Exp]
  | ELam (Bind [Pat] Exp)
  | ELet (Bind (Rec [(Pat, Embed Exp)]) Exp)
  | EIf Exp Exp Else
  | ECase Exp [Clause]

  | ERef Exp
  | EDeref Exp
  
  | ECon String [Exp]
  | ENewCon String [Exp]
  | EFree Exp

  | EGet Exp String
  | EGetI Exp Exp  -- For arrays only
  | ESet Exp Exp

  | ENewArray [Exp]
  | ENewArrayI Exp
  | EResizeArray Exp Exp

  | ENewString String
  | ENewStringI Exp

  | EOp Op
  deriving (Show, Generic, Typeable)

-- Literals
data Lit
  = LInt Int
  | LDouble Double
  | LChar Char
  | LBool Bool
  | LString String
  | LStringI Exp
  | LArray [Exp]
  | LArrayI Exp
  deriving (Show, Generic, Typeable)

-- Else Branches
data Else
  = Else Exp
  | Elif Exp Exp Else
  deriving (Show, Generic, Typeable)

-- Operations
data Op
  = OpAddI Exp Exp
  | OpSubI Exp Exp
  | OpMulI Exp Exp
  | OpDivI Exp Exp
  | OpRemI Exp Exp
  | OpNeg Exp
  
  | OpAddF Exp Exp
  | OpSubF Exp Exp
  | OpMulF Exp Exp
  | OpDivF Exp Exp
  | OpRemF Exp Exp

  | OpAnd Exp Exp
  | OpOr  Exp Exp
  | OpXor Exp Exp

  | OpEqI Exp Exp
  | OpNeqI Exp Exp
  | OpLT Exp Exp
  | OpGT Exp Exp
  | OpGE Exp Exp
  | OpLE Exp Exp
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

elam :: [Pat] -> Exp -> Exp
elam ps body = ELam (bind ps body)

---------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------

data Type
  = TArr Type Type
  | TCon String
  | TI8
  | TI32
  | TI64
  | TF32
  | TF64
  | TBool
  | TChar
  | TArray Int Type 
  | TPtr Type
  | TString
  | TVoid
  deriving (Show, Generic, Typeable)

-- Type Equality

instance Eq Type where
  (==) a b = case (a, b) of
    (TArr a1 a2, TArr b1 b2) -> (a1 == b1) && (a2 == b2)
    (TCon n1, TCon n2) -> n1 == n2
    
    
    (TI8, TI8)   -> True
    (TI32, TI32) -> True
    (TI64, TI64) -> True
    (TF32, TF32) -> True
    (TF64, TF64) -> True

    (TBool, TBool) -> True

    (TChar, TChar) -> True
    (TI8, TChar) -> True
    (TChar, TI8) -> True

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

exPType :: Pat -> Type
exPType (PType _ ty) = ty
exPType _ = error "Expected typed expression!"

exElseType :: Else -> Type
exElseType = \case
  Else e -> exType e
  Elif p t f -> unify (exType t) (exElseType f)


inttypes :: [Type]
inttypes = [TI8, TI32]

-- Unification for typecking.
-- Checks if types are equal.
-- Throws an error if not.
-- Returns the most specific type.
unify :: Type -> Type -> Type
unify (TPtr t1) (TArray n t2)
  | t1 == t2 = TArray n t2
  | otherwise = unify_err (TPtr t1) (TArray n t2)

unify (TArray n t1) (TPtr t2)
  | t1 == t2 = TPtr t2
  | otherwise = unify_err (TArray n t1) (TPtr t2)

unify (TPtr TI8) TString = TString
unify TString (TPtr TI8) = TPtr TI8

unify TI8 TChar = TChar
unify TChar TI8 = TI8

unify (TPtr t1) (TPtr t2)
  | t1 == t2 = TPtr (unify t1 t2)
  | otherwise = unify_err (TPtr t1) (TPtr t2)

unify t1 t2
  | t1 == t2 = t2
  | otherwise = unify_err t1 t2


unify_err :: Type -> Type -> Type
unify_err t1 t2 = error $ "Type mismatch!\n"
                     ++ "Expected: " ++ show t1 ++ "\n"
                     ++ "Actual: " ++ show t2 ++ "\n"



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

clause :: Pat -> Exp -> Clause
clause p e = Clause (bind p e)

exClauseBody :: Fresh m => Clause -> m Exp
exClauseBody (Clause bnd)
  = snd <$> unbind bnd


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
instance Alpha Lit
instance Alpha Else
instance Alpha Type
instance Alpha Func
instance Alpha Pat
instance Alpha Clause
instance Alpha Op
instance Alpha a => Alpha (Telescope a)

instance Subst Exp Func 
instance Subst Exp Lit
instance Subst Exp Else
instance Subst Exp Type
instance Subst Exp Pat
instance Subst Exp Clause
instance Subst Exp Op
instance Subst Exp a => Subst Exp (Telescope a)
instance Subst Exp Exp where
  isvar (EVar v) = Just (SubstName v)
  isvar _  = Nothing