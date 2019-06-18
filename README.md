# Type Theory Compiler

## About

This compiler can lower a type theoretic lambda calculus into efficient LLVM IR code.
Each expression is strictly evaluated, and function calls are call-by-value.
The languages provide primitives for pointers, arrays, and c-strings. It even includes
stack and heap managment, and allow for mutation and unchecked side-effects.
These are dangerous, but the theories provided are intented as compiler targets
for more advanced theories, which can type-check on usages and side-effects. 
So far, the theories provided are Simply-Typed Lambda Calculus (STLC) and SystemF.

This code is in active development, and many features are unimplemented.
In the future, we hope to have a dependent type theory of some kind,
which is capable of checking resource usage. The current candidate theory
is Quantitative Type Theory (QTT).
Another subject of interest is laziness. We may look into an alternate
backend which will compile to lazy, garbage collected code. Another area
of interest is interactions between theories.


## Requirements

You will need llvm-8 and the latest ghc/cabal for compiling haskell.