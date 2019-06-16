{-# LANGUAGE LambdaCase,
             FlexibleInstances, 
             MultiParamTypeClasses, 
             FlexibleContexts,
             DeriveGeneric,
             DeriveDataTypeable
  #-}
module Syntax where

import Data.Semiring
import Data.Text (Text, pack)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Unbound.Generics.LocallyNameless

data R
  = RZero
  | ROne
  | RMany
  deriving (Show, Generic, Typeable)

instance Semiring R where
  plus RZero RZero = RZero
  plus RZero ROne  = ROne
  plus RZero RMany = RMany
  plus ROne  RZero = ROne
  plus ROne  ROne  = RMany
  plus ROne  RMany = RMany
  plus RMany RZero = RMany
  plus RMany ROne  = RMany
  plus RMany RMany = RMany

  times RZero RZero = RZero
  times RZero ROne  = RZero
  times RZero RMany = RZero
  times ROne  RZero = RZero
  times ROne  ROne  = ROne
  times ROne  RMany = RMany
  times RMany RZero = RZero
  times RMany ROne  = RMany
  times RMany RMany = RMany

  zero = RZero
  one = ROne

  fromNatural n
    | n == 0 = RZero
    | n == 1 = ROne
    | True   = RMany



type TName = Name Term
type CName = String

name2Text :: Name a -> Text
name2Text = pack . name2String

data Context = Map Text (R, Type)

type Type = Term

data Term
  = TType
  | TUnit
  | TAnnot Term Type

  | TVar TName
  | TPi R (Bind (TName, Embed Type) Type)

  | TLam R (Bind (TName, Embed (Maybe Type)) Term)
  | TApp Term Term
  
  | TProd R (Bind (TName, Embed Type) Type)
  | TPair Term Term
  | TFst Term
  | TSnd Term
  
  | TLet (Bind (TName, TName, Embed Term) Term)
  | TSeq Term Term
  
  | TBool
  | TTrue
  | TFalse
  | TIf Term Term Term

  | TCon CName  
  | TNew CName
  | TFree Term
  deriving (Show, Generic, Typeable)


data Value
  = VUnit
  | VType
  | VInt Integer
  | VChar Char
  | VString String
  | VCon CName [Value]
  | VBool Bool
  | VBoolType
  | VProd (Bind (TName, Embed Type) Type)
  | VPair Value Value
  | VLam (Bind (TName, Embed (Maybe Type)) Term)
  | VPi (Bind (TName, Embed Type) Type)
  deriving (Show, Generic, Typeable)

-------------------------------------------------------
-- Instances for substitution and alpha equality
-------------------------------------------------------

instance Alpha R
instance Alpha Term
instance Alpha Value

instance Subst Term R
instance Subst Term Value 
instance Subst Term Term where
  isvar (TVar v) = Just (SubstName v)
  isvar _  = Nothing