#!/bin/bash

cabal new-run anf-llvm-test \
  && clang-8 -O1 test.ll -o test \
  && clang-8 -O1 -emit-llvm -S test.ll -o test-opt.ll \
  && echo "running test..." && ./test