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


## Useful Links

Some links to help with development

### Parsing
https://www.haskell.org/alex/doc/html/index.html

https://www.haskell.org/happy/doc/html/index.html (anyone have a link for v1.19 docs?)

https://github.com/harpocrates/language-rust/blob/0e02e21c389cb56c77f71b902288b195ed273af6/src/Language/Rust/Parser/Internal.y#L74


### Pattern Matching Compiler
http://l-lang.org/blog/Compiling-pattern-matching/


### LLVM
http://hackage.haskell.org/package/llvm-hs

http://hackage.haskell.org/package/llvm-hs-pure

https://mapping-high-level-constructs-to-llvm-ir.readthedocs.io/en/latest/README.html


### Destination-Passing Style
https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/dps-fhpc17.pdf


### Monomorphization
https://reasonablypolymorphic.com/blog/algorithmic-sytc/


### Arbitrary Rank Polymorphic Type Checking
https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/putting.pdf

https://github.com/namin/higher-rank

https://github.com/garyb/infer-rank-n-types


### Bidirectional Type Checking
https://www.cl.cam.ac.uk/~nk480/bidir.pdf

https://github.com/ollef/Bidirectional

http://davidchristiansen.dk/tutorials/bidirectional.pdf

https://github.com/sweirich/pi-forall


### Tridirectional Type Checking
https://www.cs.cmu.edu/~fp/papers/popl04.pdf


### Quantitative Type Theory
https://github.com/LightAndLight/qtt