module Language.SystemF.Monomorphize where

-- This will monomorphize polymorphic functions by
-- generating monomorphic instances of each function.

-- This pass requires partial, cconv, and lambda lifting.

-- This will produce STLC.