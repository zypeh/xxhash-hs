module Data.Hash.XXHash where

import Data.Word
import qualified Data.ByteString as B

import Data.Hash.XXHash.Internal
import System.IO.Unsafe (unsafePerformIO)

hash' :: B.ByteString -> Word64
hash' bs = unsafePerformIO . B.useAsCStringLen bs $ \(str, len) ->
  pure $ c_XXH3_64bits str (fromIntegral len)