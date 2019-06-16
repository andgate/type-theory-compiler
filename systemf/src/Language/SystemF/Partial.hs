module Language.SystemF.Partial where

-- We can implement partial applications by identifying
-- and wrapping them in lambdas expressions.

-- Later phases will lift these functions and handle closure conversion.
-- This will lead to a function new function generated for each
-- partial application, which should be efficient enough to compete with C.