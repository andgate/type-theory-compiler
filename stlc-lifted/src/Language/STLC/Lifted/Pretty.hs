{-# LANGUAGE LambdaCase
           , OverloadedStrings
           #-}
module Language.STLC.Lifted.Pretty where

import Language.STLC.Lifted.Syntax

import Unbound.Generics.LocallyNameless

import Data.Text.Prettyprint.Doc

instance Pretty Defn where
  pretty = \case
    FuncDefn f -> undefined

instance Pretty Type where
  pretty = \case
    TArr a b -> pretty a <+> "->" <+> pretty b
    TCon n -> pretty n

