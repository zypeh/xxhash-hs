{-# LANGUAGE MagicHash, CPP #-}
module Data.Hash.XXH3 where

import qualified Data.ByteString.Internal as B
import GHC.ForeignPtr
import GHC.Exts
import Data.Bits
import Foreign.Storable
import GHC.Word
import Foreign.Ptr

peek32le :: Ptr Word8 -> Int -> IO Word32
peek32le ptr offset = do
  let peek8le = peekByteOff ptr
  b0 <- peek8le 0
  b1 <- peek8le 1
  b2 <- peek8le 2
  b3 <- peek8le 3
  let w = (b3 `shiftL` 24) .|.
          (b2 `shiftL` 16) .|.
          (b1 `shiftL`  8) .|.
          b0
  return w
{-# INLINE peek32le #-}

peek64le :: Ptr Word8 -> Int -> IO Word64
peek64le ptr offset = do
  let peek8le = peekByteOff ptr
  b0 <- peek8le 0
  b1 <- peek8le 1
  b2 <- peek8le 2
  b3 <- peek8le 3
  b4 <- peek8le 4
  b5 <- peek8le 5
  b6 <- peek8le 6
  b7 <- peek8le 7
  let w = (b7 `shiftL` 56) .|.
          (b6 `shiftL` 48) .|.
          (b5 `shiftL` 40) .|.
          (b4 `shiftL` 32) .|.
          (b3 `shiftL` 24) .|.
          (b2 `shiftL` 16) .|.
          (b1 `shiftL`  8) .|.
          b0
  return w
{-# INLINE peek64le #-}

-- XXH3_64bits_internal
xxh364 :: B.ByteString -> Word64 -> IO Word64
xxh364 bs@(B.PS fp@(ForeignPtr ptr x) (I# offset) (I# len)) seed
  | isTrue# (len <=# 3#) = undefined -- hash_len_1to3
  | isTrue# (len <=# 8#) = hash_len_4to8
  | isTrue# (len ># 8#) = hash_len_9to16
  | isTrue# (len <=# 128#) = hash_len_17to128
  | isTrue# (len <=# 240#) = hash_len_129to240
  | otherwise = hash_long
  where
    secret = 0x396cfeb8 :: Word32
    secretPlus4 = 0xbe4ba423 :: Word32
    secretPlus8 = 0x1cad21f72c81017c :: Word64
    secretPlus16 = 0xdb979083e96dd4de :: Word64

    hash_len_1to3 = do
      let c1 = indexInt8OffAddr# ptr 0#
      let c2 = indexInt8OffAddr# ptr (len `iShiftRL#` 1#)
      let c3 = indexInt8OffAddr# ptr (len -# 1#)

      let combined = (I# c1 `shiftL` 16) .|.
                     (I# c2 `shiftL` 24) .|.
                     (I# c3 `shiftL`  0) .|.
                     (I# len `shiftL` 8)

      let bitflip = fromIntegral (secret `xor` secretPlus4) + seed
      let keyed = fromIntegral combined `xor` fromIntegral bitflip
      xxhash64Avalanche keyed

    hash_len_4to8 = do
      let seed' = seed `xor` fromIntegral (byteSwap32 (fromIntegral seed))
      
      let ptrWord = unsafeForeignPtrToPtr fp
      input1 <- peek32le ptrWord 0
      input2 <- peek32le (ptrWord `plusPtr` I# (len -# 4#)) 0

      let bitflip = (secretPlus8 `xor` secretPlus16) - seed'
      let input64 = fromIntegral input2 + ((fromIntegral input1) `shiftL` 32)
      let keyed = input64 `xor` bitflip
      return $ xxhash3_rrmxmx keyed (fromIntegral (I# len))

    hash_len_9to16 = undefined
    hash_len_17to128 = undefined
    hash_len_129to240 = undefined 
    hash_long = undefined

-- XXH64_avalanche
xxhash64Avalanche :: Word64 -> Word64
xxhash64Avalanche input = do
  let i1 = input `xor` (input `shiftR` 33)
  let i2 = 0xC2B2AE3D27D4EB4F * i1
  let i3 = i2 `xor` (i2 `shiftR` 29)
  let i4 = i3 * 0x165667B19E3779F9
  let i5 = i4 `xor` (i4 `shiftR` 32)
  i5

-- | This is a stronger avalance, inspired by Pelle Evensen's rrmxmx
-- prefaranble when input has not been previously mixed.
xxhash3_rrmxmx :: Word64 -> Word64 -> Word64
xxhash3_rrmxmx x len = do
  let k = 0x9FB21C651E98DF25 :: Word64
  let i1 = x `xor` (rotateL x 49 `xor` rotateL x 24)
  let i2 = i1 * k
  let i3 = i2 `xor` ((i2 `shiftR` 35) + len)
  let i4 = i3 * k
  xorShift64 i4 28

xorShift64 :: Word64 -> Int -> Word64
xorShift64 v64 s = v64 `xor` (v64 `shiftR` s)