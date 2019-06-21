module Language.STLC.Lifted.Eval where

import Language.STLC.Syntax

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Env = Map String Exp

type Val = Lit' Val

eval :: Exp -> Either Exp Val
eval = undefind