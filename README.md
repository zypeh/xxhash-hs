# xxhash-hs

> This is a learn-by-doing project.

There are a few haskell xxhash package in hackage, and here is the comparison of them:

- [xxhash-ffi](https://github.com/haskell-haskey/xxhash-ffi) It is using older version of XXHash and connect with GHC FFI. It is the fastest FFI binding because it used a lot [of](https://github.com/haskell-haskey/xxhash-ffi/blob/master/src/Data/Digest/XXHash/FFI.hs#L31-L33) [tricks](https://github.com/haskell-haskey/xxhash-ffi/blob/master/src/Data/Digest/XXHash/FFI.hs#L35-L37) and specializer pragma.

- [xxhash](https://github.com/christian-marie/xxhash) It is the only xxhash haskell pacakge that is officially [stated](https://code.google.com/archive/p/xxhash/) because it is implemented purely in Haskell!

Both of them are missing the XXHash3 variant which got its API stabled recently.

### What am I going to do
- Learn and ~~steal~~ implement optimization techniques from `xxhash-ffi` and implement more instances like `Hashable`.

- Implement a hashmap using this library.
