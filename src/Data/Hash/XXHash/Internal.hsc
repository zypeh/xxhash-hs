{-# LANGUAGE CPP #-}
module Data.Hash.XXHash.Internal where

import Data.Word
import Foreign.C.String
import Foreign.C.Types


#include "xxhash.h"
-- foreign import ccall "func" c_func :: Int -> Int

-- | XXH3 is a new hash algorithm featuring:
-- - Improved speed for both small and large inputs
-- - true 64-bit and 128-bit outputs
-- - SIMD acceleration
-- - Improved 32-bit viability

-- default 64-bit variant, use default secret and default seed of 0.
foreign import ccall unsafe "XXH3_64bits"
  c_XXH3_64bits :: CString -> CSize -> Word64