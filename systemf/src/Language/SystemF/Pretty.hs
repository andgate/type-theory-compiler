{-# LANGUAGE LambdaCase
           , OverloadedStrings
           , FlexibleInstances
           #-}
module Language.SystemF.Pretty where

import Language.SystemF.Syntax

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Name

import Data.Text.Prettyprint.Doc