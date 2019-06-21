module Language.STLC.Eval where

import Language.STLC.Syntax

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Env = Map String Exp

eval :: Exp -> Either Exp Lit
eval = undefined