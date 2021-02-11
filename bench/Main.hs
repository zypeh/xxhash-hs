module Main where

import Gauge
import Control.Monad
import qualified Data.ByteString.Char8 as B

import qualified Data.Hashable as H
import Data.Digest.XXHash
import System.Random
import Data.Hash.XXH3

numbersGen :: IO [B.ByteString]
numbersGen = do
    xs <- replicateM 33333 (randomRIO (0, 100) :: IO Int)
    pure $ B.pack . show <$> xs

main :: IO ()
main = do
    numbers <- numbersGen
    defaultMain [
        bgroup "Numbers" [
            bench "Data.Hash.XXHash3.FFI" $ whnf (map $ \a -> xxh364 a 0) numbers,
            bench "Data.Hashable.hash" $ whnf (map H.hash) numbers,
            bench "Data.Digest.XXHash Strict" $ whnf (map xxHash') numbers
        ]
     ]