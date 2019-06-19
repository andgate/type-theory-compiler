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
backend which will compile to lazy, garbage collected code. Another area
of interest is interactions between theories.


## Requirements

You will need llvm-8 and the latest ghc/cabal for compiling haskell.
Also, you should run `cabal update` to ensure you have the freshest
set of packages for haskell.

## Running the test

The current test compiler is tcc. Simply call `cabal new-run tcc`.
This will run some tests and produce an llvm ir file, `test.ll`.
You can then compile this file with `clang -O1 test.ll -o test`, and this
will produce a binary executable, which you can run with `./test`.

The script `runtest.sh` can do this for you and even produces optimized
llvm ir code in the file `test.ll.opt`.
