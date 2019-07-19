{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase,
             FlexibleInstances, 
             MultiParamTypeClasses, 
             FlexibleContexts,
             DeriveGeneric,
             DeriveDataTypeable,
             DataKinds,
             GADTs,
             KindSignatures,
             StandaloneDeriving,
             ViewPatterns
          #-}
module Language.STLC.Syntax where

import Language.Syntax.Location

import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty, (<|))
import qualified Data.List.NonEmpty as NE
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Unbound.Generics.LocallyNameless


-- The Simply Typed Lambda Calculus
--   Invariants: Fully type annotated
--             , Lambda Lifted
--             , Monomorphic
--             , No Partial Application

type Var = Name Exp

data Module = Module Loc String [Defn]

data Defn
  = FuncDefn Func
  | ExternDefn Extern
  | DataTypeDefn DataType
  deriving (Show, Generic, Typeable)

data Extern = Extern Loc String [Type] Type
  deriving (Show, Generic, Typeable)

-- This could use a refactor of some kind
data DataType =
  DataType Loc String [ConstrDefn]
  deriving (Show, Generic, Typeable)

data ConstrDefn
  = ConstrDefn Loc String [Type]
  | RecordDefn Loc String (NonEmpty Entry)
  deriving (Show, Generic, Typeable)

constrName :: ConstrDefn -> String
constrName = \case
  ConstrDefn _ n _ -> n
  RecordDefn _ n _ -> n

constrArity :: ConstrDefn -> Int
constrArity = \case
  ConstrDefn _ _ tys -> length tys
  RecordDefn _ _ ens -> NE.length ens

getEntries :: ConstrDefn -> [Entry]
getEntries = \case
  ConstrDefn _ _ _ -> []
  RecordDefn _ _ ens -> NE.toList ens 


data Entry = Entry Loc String Type
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

data Func = Func Loc Type String (Bind [Pat] Exp)
    deriving (Show, Generic, Typeable)


func :: Loc -> String -> Type -> [Pat] -> Exp -> Func
func l n ty ps body = Func l ty n (bind ps body)

---------------------------------------------------------------------------
-- Expressions
---------------------------------------------------------------------------

data Exp
  = EVar Var
  | ELit Lit
  | EApp Exp (NonEmpty Exp)
  
  | EType Exp Type
  | ECast Exp Type
  
  | ELoc Exp Loc
  | EParens Exp

  | ELam (Bind (NonEmpty Pat) Exp)
  | ELet (Bind (Rec (NonEmpty (Pat, Embed Exp))) Exp)
  | EIf Exp Exp Else
  | ECase Exp (NonEmpty Clause)

  | ERef Exp
  | EDeref Exp
  
  | ETuple Exp (NonEmpty Exp)
  | ECon String [Exp]
  | ENewCon String [Exp]
  | EFree Exp

  | EGet Exp String
  | EGetI Exp Exp  -- For arrays only
  | ESet Exp Exp

  | ENewArray [Exp]
  | ENewArrayI Exp
  | EResizeArray Exp Exp

  | ENewVect [Exp]
  | ENewVectI Exp

  | ENewString String

  | EOp Op
  deriving (Show, Generic, Typeable)

exEAnn :: Exp -> Exp
exEAnn = \case
  ELoc e _ -> e
  EType e _ -> e
  e -> e 

-- Literals
data Lit
  = LNull
  | LBool Bool
  | LInt Integer
  | LDouble Double
  | LChar Char
  | LString String
  | LArray [Exp]
  | LArrayI Exp
  | LVect [Exp]
  | LVectI Exp
  deriving (Show, Generic, Typeable)

-- Else Branches
data Else
  = Else (Maybe Loc) Exp
  | Elif (Maybe Loc) Exp Exp Else
  deriving (Show, Generic, Typeable)

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
  | OpGT Exp Exp
  | OpGE Exp Exp
  | OpLE Exp Exp
  deriving (Show, Generic, Typeable)


--Helpers

evar :: String -> Exp
evar = EVar . s2n

eapp :: String -> [Exp] -> Exp
eapp f_n xs = EApp (evar f_n) (NE.fromList xs) 

elet :: [(Pat, Exp)] -> Exp -> Exp
elet qs body = ELet (bind (rec $ NE.fromList (second embed <$> qs)) body)

ecase :: Exp -> [(Pat, Exp)] -> Exp
ecase e qs = ECase e (uncurry clause <$> NE.fromList qs)

elam :: [Pat] -> Exp -> Exp
elam ps body = ELam (bind (NE.fromList ps) body)

---------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------

data Type
  = TArr Type Type
  | TCon String
  | TInt Int
  | TUInt Int
  | TFp Int
  | TTuple Type (NonEmpty Type)
  | TArray Int Type 
  | TVect Int Type
  | TPtr Type
  | TLoc Type Loc
  | TParens Type
  deriving (Show, Generic, Typeable)


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
    TArr _ _ -> True
    TArray _ _ -> True
    TPtr _ -> True
    _ -> False


-- Type Equality

instance Eq Type where
  (==) a b = case (a, b) of
    (TArr a1 a2, TArr b1 b2) ->
      (a1 == b1) && (a2 == b2)
    
    (TCon n1, TCon n2) -> n1 == n2
    
    (TInt  s1, TInt  s2) -> s1 == s2
    (TUInt s1, TUInt s2) -> s1 == s2
    (TFp   s1, TFp   s2) -> s1 == s2

    (TVect s1 t1, TVect s2 t2)
      -> s1 == s2 && t1 == t2

    (TPtr t1, TPtr t2) -> t1 == t2

    (TTuple t1 t1s, TTuple t2 t2s)
      -> (t1<|t1s) == (t2<|t2s)

    (TArray i1 t1, TArray i2 t2)
      -> (i1 == i2) && (t1 == t2)   

    (TArray _ t1, TPtr t2) -> t1 == t2
    (TPtr t1, TArray _ t2) -> t1 == t2

    (TLoc t1 _, t2) -> t1 == t2
    (t1, TLoc t2 _) -> t1 == t2

    (TParens t1, t2) -> t1 == t2 
    (t1, TParens t2) -> t1 == t2 

    _ -> False


-- Helpers

tarr :: [Type] -> Type -> Type
tarr paramtys retty
  = foldr (\a b -> TArr a b) retty paramtys

exTyAnn :: Type -> Type
exTyAnn = \case
  TLoc t _ -> exTyAnn t
  TParens t -> exTyAnn t
  t -> t

exType :: Exp -> Type
exType (EType _ ty) = ty
exType (ECast _ ty) = ty
exType (ELoc e _) = exType e
exType (EParens e) = exType e
exType _ = error "Expected typed expression!"

exPType :: Pat -> Type
exPType (PType _ ty) = ty
exPType (PLoc p _) = exPType p
exPType (PParens p) = exPType p
exPType _ = error "Expected typed expression!"

exElseType :: Else -> Type
exElseType = \case
  Else _ e -> exType e
  Elif _ _ (exType -> t) (exElseType -> f)
    | t == f -> t
    | otherwise -> error $ "Expected both branches of else to have same type"




---------------------------------------------------------------------------
-- Patterns
---------------------------------------------------------------------------

data Pat
  = PVar Var
  | PCon String [Pat]
  | PTuple Pat (NonEmpty Pat)
  | PWild
  | PType Pat Type
  | PLoc Pat Loc
  | PParens Pat
  deriving (Show, Generic, Typeable)


exPAnn :: Pat -> Pat
exPAnn = \case
  PType p _ -> exPAnn p
  PLoc p _ -> exPAnn p
  PParens p -> exPAnn p
  p -> p


-- Clauses (Case branches)
data Clause = Clause (Maybe Loc) (Bind Pat Exp)
  deriving (Show, Generic, Typeable)

clause :: Pat -> Exp -> Clause
clause p e = Clause (Just (p <++> e)) (bind p e)

exClauseBody :: Fresh m => Clause -> m Exp
exClauseBody (Clause _ bnd)
  = snd <$> unbind bnd


-- Helpers

pvar :: String -> Pat
pvar = PVar . s2n

patTypedVars :: Pat -> [(String, Type)]
patTypedVars p = patTypedVars' (exPType p) p

patTypedVars' :: Type -> Pat -> [(String, Type)]
patTypedVars' ty = \case
  PVar v -> [(name2String v, ty)]
  PCon _ ps -> concatMap patTypedVars ps
  PTuple p (NE.toList -> ps) ->
    concatMap patTypedVars (p:ps)
  PWild -> []
  PType p ty' -> patTypedVars' ty' p
  PLoc p _ -> patTypedVars' ty p
  PParens p -> patTypedVars' ty p


splitType :: Type -> ([Type], Type)
splitType ty =
  case exTyAnn ty of
    TArr t1 t2 ->
      let (ts, t2') = splitType t2
      in (t1:ts, t2')
    _ -> ([], ty)


---------------------------------------------------------------------------
-- Location instances
---------------------------------------------------------------------------

instance HasLocation ConstrDefn where
  locOf = \case
    ConstrDefn l _ _ -> l
    RecordDefn l _ _ -> l

instance HasLocation Entry where
  locOf (Entry l _ _) = l

instance HasLocation Exp where
  locOf = \case
    ELoc _ l -> l
    EType e _ -> locOf e
    EParens e -> locOf e
    _ -> error $ "expected located expression!"

instance HasLocation Else where
  locOf = \case
    Else (Just l) _ -> l
    Else Nothing _ -> error $ "expected located else-branch!"
    
    Elif (Just l) _ _ _ -> l
    Elif Nothing _ _ _ -> error $ "expected located elif-branch!"

instance HasLocation Clause where
  locOf (Clause (Just l) _) = l
  locOf (Clause Nothing _)
    = error $ "expected located clause!"

instance HasLocation Type where
  locOf = \case
    TLoc _ l -> l
    TParens t -> locOf t
    _ -> error $ "expected located type!"

instance HasLocation Pat where
  locOf = \case
    PLoc _ l -> l
    PParens p -> locOf p
    _ -> error $ "expected located pattern!"


---------------------------------------------------------------------------
-- Alpha and Subst instances for unbound-generics
---------------------------------------------------------------------------

instance Alpha Exp
instance Alpha Lit
instance Alpha Else
instance Alpha Type
instance Alpha Func
instance Alpha Loc
instance Alpha Region
instance Alpha Position
instance Alpha Pat
instance Alpha Clause
instance Alpha Op
instance Alpha a => Alpha (Telescope a)
instance Alpha a => Alpha (NonEmpty a)

instance Subst Exp Func 
instance Subst Exp Loc
instance Subst Exp Region
instance Subst Exp Position
instance Subst Exp Lit
instance Subst Exp Else
instance Subst Exp Type
instance Subst Exp Pat
instance Subst Exp Clause
instance Subst Exp Op
instance Subst Exp a => Subst Exp (Telescope a)
instance Subst Exp a => Subst Exp (NonEmpty a)
instance Subst Exp Exp where
  isvar (EVar v) = Just (SubstName v)
  isvar _  = Nothing