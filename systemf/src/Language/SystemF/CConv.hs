module Language.SystemF.CConv where

-- Closure conversion is done by find lambda terms,
-- and adding a closure as the first argument of a function.
-- A lambda expression with a closure is considered saturated.
-- Since the function is a local variable, applications of
-- it must be found and updated with their closure as the
-- first argument.

-- Lambda lifting depends on closure conversion.
-- Lambda's lifted before cconv will have 
-- varibles that are never introduced into scope.