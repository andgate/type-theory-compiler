# Type Theory Compiler

## About

This compiler can lower a type theoretic lambda calculus into efficient LLVM IR code.
Each expression is strictly evaluated, and functions are call-by-value.
The languages provide primitives for pointers, arrays, and c-strings. It even includes
stack and heap managment, and allows for mutation and unchecked side-effects.
These are dangerous, but the theories provided are intented as compiler targets
for more advanced theories, which can type-check on usages and side-effects. 
So far, the theories provided are Simply-Typed Lambda Calculus (STLC) and SystemF.

This code is in active development, and many features are unimplemented.
In the future, we hope to have a dependent type theory of some kind,
which is capable of checking resource usage. The current candidate theory
is Quantitative Type Theory (QTT).
Another subject of interest is laziness. We may look into an alternate
backend which will compile to lazy, garbage collected code.
Linear languages may also be a possible future target.
Currently, it's not clear how languages should interact.


## Requirements

You will need llvm-8 and the latest ghc/cabal for compiling haskell.
Also, you should run `cabal update` to ensure you have the freshest
set of packages for haskell.

## Running tests

To run tests, run the command `cabal new-test ttc-golden`.

## Building files

Currently, only `.stlc` files are supported. To build files,
use the command:
```
cabal new-run ttc -- [-o outfile] infile..
```
If the build succeeds, you may execute the outfile.
The default outfile is `a.out`.

## Languages

We have a family of languages:
  - LLTT: Low Level Type Theory, a calculus that easily compiles to llvm ir code.
  - STLC: Simply Type Lambda Calculus
  - SystemF: STLC + type variables and polymorphism
