{-# LANGUAGE LambdaCase
           , OverloadedStrings
           #-}
module Language.LLTT.Pretty where

import Language.LLTT.Syntax

import Data.Text.Prettyprint.Doc

instance Pretty Defn where
  pretty = \case
    FuncDefn f -> undefined

instance Pretty Type where
  pretty = \case
    TFunc retty paramty -> pretty paramty <+> "->" <+> pretty retty
    TCon n -> pretty n
