module Language.STLC.Lifted.Match where

-- This pass should remove all nested patterns.

import Language.STLC.Lifted.Syntax
import Unbound.Generics.LocallyNameless

type Equation = ([Pat], Exp)

match :: Free m => [Exp] -> [([Pat], Exp)] -> Exp -> m ()
match _ [([], e)] _ = return e