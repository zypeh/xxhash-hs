{-# LANGUAGE CPP #-}
module Data.Digest.XXHash3.Internal where

#include "xxhash.h"
-- foreign import ccall "func" c_func :: Int -> Int
