# bytestring-encodings

This library aims to provide an efficient way to determine if a given ByteString
is encoded according to a certain encoding scheme, e.g. ASCII or UTF8.

Currently only an efficient ASCII implementation is available; it is consistently
8x faster than the naive implementation of checking that each byte is less than
128 (see benchmarks). If compiling with LLVM, this can be made even faster with
vectorising (see [GHC's Operations on SIMD Vectors](https://hackage.haskell.org/package/ghc-prim-0.5.1.1/docs/GHC-Prim.html#g:29)).

UTF8 is next to be implemented.

PRs welcome if anyone would like to add support for other encodings. 
