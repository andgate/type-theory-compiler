module Language.SystemF.Lift where

-- Lambda lifting finds nested function definitions,
-- generates a name for them and lifts them out of a
-- function body. All lambda expressions are replaced
-- by a pointer to their lifted version.

-- This pass requires partial and cconv.

import Language.SystemF.Syntax

lift :: Module -> Module
lift = undefined