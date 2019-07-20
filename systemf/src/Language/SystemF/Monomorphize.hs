{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Language.SystemF.Monomorphize where

import Language.SystemF.Syntax
import qualified Language.STLC.Syntax as ST

-- This will monomorphize polymorphic functions by
-- generating monomorphic instances of each function.

-- This pass requires partial, cconv, and lambda lifting.

-- This will produce STLC.

monomorphize :: Module -> ST.Module
monomorphize modl = undefined