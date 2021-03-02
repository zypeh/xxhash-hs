{-# LANGUAGE MagicHash #-}
module Main where

import Gauge
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Numeric
import qualified Data.Hashable as H
import Data.Digest.XXHash
import System.Random
import Data.Hash.XXH3

-- import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import GHC.ForeignPtr 
import GHC.Exts (Addr#, Int(I#), plusAddr#, eqAddr#)
import Data.Word (Word8, Word64)
import Data.Bits
import Data.Primitive.Types (indexOffAddr#)

fnv1a64 :: B.ByteString -> Word64
fnv1a64 (B.PS (ForeignPtr ptr _) (I# offset) (I# len)) = go start end offsetBasis
  where
    offsetBasis = 14695981039346656037
    fnvPrime = 1099511628211
    start = plusAddr# ptr offset
    end = plusAddr# start len

    go :: Addr# -> Addr# -> Word64 -> Word64
    go ptr end hash = case eqAddr# ptr end of
      1# -> hash
      _  -> go (plusAddr# ptr 1#) end $ do
        let byte = indexOffAddr# ptr 0# :: Word8
        (hash `xor` fromIntegral byte) * fnvPrime
{-# INLINE fnv1a64 #-}

numbersGen :: IO [B.ByteString]
numbersGen = do
    xs <- replicateM 33333 (randomRIO (0, 100) :: IO Int)
    pure $ B.pack . show <$> xs

stringsGen :: IO [B.ByteString]
stringsGen = do
    xs <- replicateM 33333 (randomRIO (10000, 99999) :: IO Int)
    pure $ B.pack . (`showHex` "") <$> xs

main :: IO ()
main = do
    numbers <- numbersGen
    strings <- stringsGen
    defaultMain [
        bgroup "Numbers" [
            bench "Data.Hash.XXH3" $ whnf (map $ \a -> xxh364 a 0) numbers,
            bench "Data.Hashable.hash" $ whnf (map H.hash) numbers,
            bench "Data.Digest.XXHash Strict" $ whnf (map xxHash') numbers,
            bench "FNV-1a" $ whnf (map fnv1a64) numbers
        ],
        bgroup "Strings" [
            bench "Data.Hash.XXH3" $ whnf (map $ \a -> xxh364 a 0) strings,
            bench "Data.Hashable.hash" $ whnf (map H.hash) strings,
            bench "Data.Digest.XXHash Strict" $ whnf (map xxHash') strings,
            bench "FNV-1a" $ whnf (map fnv1a64) strings
        ]

     ]