module Data.Hash.XXHash3 where

import Data.Word
import qualified Data.ByteString as B

import Data.Hash.XXHash.Internal
import System.IO.Unsafe (unsafePerformIO)

hash' :: B.ByteString -> IO Word64
hash' bs = B.useAsCStringLen bs $ \(str, len) ->
  pure $ c_XXH3_64bits str (fromIntegral len)
