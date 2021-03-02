# xxhash-hs

**(incomplete)** xxHash3 implementation written in Haskell.

This repo contains the undone pure Haskell XXH3 implementation which only accepts input shorter than 241 bytes. The implementation to handle longer bytes require me to grok on the XXhash C code which is written in SSE/AVX instructions.

Observation from the implementation:

* `fromIntegral` in Haskell is actually quite effecient as it only coerce the numeric type from one to another.
* The only array-like data structure that is compatible with C is `Data.Array.Storable`. It requies the IO monad, so when dealing with pointer arithematics we have to use `unsafePerformIO`.
* The XXhash3 is fast because it handle 4 bytes at a time, which is different to FNV-1 and FNV1-a that hash bytes linearly.

**WIP, benchmark results.**

## Comparison
There are a few haskell xxhash package in hackage, and here is the comparison of them:

- [xxhash-ffi](https://github.com/haskell-haskey/xxhash-ffi) It is using older version of XXHash and connect with GHC FFI. It is the fastest FFI binding because it used a lot [of](https://github.com/haskell-haskey/xxhash-ffi/blob/master/src/Data/Digest/XXHash/FFI.hs#L31-L33) [tricks](https://github.com/haskell-haskey/xxhash-ffi/blob/master/src/Data/Digest/XXHash/FFI.hs#L35-L37) and specializer pragma.

- [xxhash](https://github.com/christian-marie/xxhash) It is the only xxhash haskell pacakge that is officially [stated](https://code.google.com/archive/p/xxhash/) because it is implemented purely in Haskell!

Both of them are missing the XXHash3 variant which got its API stabled recently.
