# Type Theory Compiler

## About

This compiler can lower a type theoretic lambda calculus into efficient LLVM IR code.
Expressions are compiled such that each expression is strictly evaluated.
Function calls are done in a call-by-value fashion, just like C. The languages provide
primitives for pointers, arrays, and c-strings. The languages even includes
stack and heap managment, and allow for mutation and unchecked side-effects.

## Requirements

You will need llvm-8 and the latest ghc/cabal for compiling haskell.