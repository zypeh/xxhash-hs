{-# LANGUAGE MagicHash, LambdaCase #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Data.Hash.XXH3 where

import qualified Data.ByteString.Internal as B
import GHC.ForeignPtr
import GHC.Exts
import Data.Bits
import Foreign.Storable
import GHC.Word
import Foreign.Ptr
import Data.Foldable
import Data.WideWord.Word128
import System.IO.Unsafe
import Data.Array.Storable

-- kSecret :: StorableArray Int Word8
kSecret :: IO (StorableArray Int Word8)
kSecret = newListArray (0,192) [
    0xb8, 0xfe, 0x6c, 0x39, 0x23, 0xa4, 0x4b, 0xbe, 0x7c, 0x01, 0x81, 0x2c, 0xf7, 0x21, 0xad, 0x1c,
    0xde, 0xd4, 0x6d, 0xe9, 0x83, 0x90, 0x97, 0xdb, 0x72, 0x40, 0xa4, 0xa4, 0xb7, 0xb3, 0x67, 0x1f,
    0xcb, 0x79, 0xe6, 0x4e, 0xcc, 0xc0, 0xe5, 0x78, 0x82, 0x5a, 0xd0, 0x7d, 0xcc, 0xff, 0x72, 0x21,
    0xb8, 0x08, 0x46, 0x74, 0xf7, 0x43, 0x24, 0x8e, 0xe0, 0x35, 0x90, 0xe6, 0x81, 0x3a, 0x26, 0x4c,
    0x3c, 0x28, 0x52, 0xbb, 0x91, 0xc3, 0x00, 0xcb, 0x88, 0xd0, 0x65, 0x8b, 0x1b, 0x53, 0x2e, 0xa3,
    0x71, 0x64, 0x48, 0x97, 0xa2, 0x0d, 0xf9, 0x4e, 0x38, 0x19, 0xef, 0x46, 0xa9, 0xde, 0xac, 0xd8,
    0xa8, 0xfa, 0x76, 0x3f, 0xe3, 0x9c, 0x34, 0x3f, 0xf9, 0xdc, 0xbb, 0xc7, 0xc7, 0x0b, 0x4f, 0x1d,
    0x8a, 0x51, 0xe0, 0x4b, 0xcd, 0xb4, 0x59, 0x31, 0xc8, 0x9f, 0x7e, 0xc9, 0xd9, 0x78, 0x73, 0x64,
    0xea, 0xc5, 0xac, 0x83, 0x34, 0xd3, 0xeb, 0xc3, 0xc5, 0x81, 0xa0, 0xff, 0xfa, 0x13, 0x63, 0xeb,
    0x17, 0x0d, 0xdd, 0x51, 0xb7, 0xf0, 0xda, 0x49, 0xd3, 0x16, 0x55, 0x26, 0x29, 0xd4, 0x68, 0x9e,
    0x2b, 0x16, 0xbe, 0x58, 0x7d, 0x47, 0xa1, 0xfc, 0x8f, 0xf8, 0xb8, 0xd1, 0x7a, 0xd0, 0x31, 0xce,
    0x45, 0xcb, 0x3a, 0x8f, 0x95, 0x16, 0x04, 0x28, 0xaf, 0xd7, 0xfb, 0xca, 0xbb, 0x4b, 0x40, 0x7e]

secret32WithOffset :: Int -> IO Word32
secret32WithOffset offset = do
  array <- kSecret
  withStorableArray array $ \ptr -> peek32le (ptr `plusPtr` offset)

secret64WithOffset :: Int -> IO Word64
secret64WithOffset offset = do
  array <- kSecret
  withStorableArray array $ \ptr -> peek64le (ptr `plusPtr` offset)

peek32le :: Ptr Word8 -> IO Word32
peek32le ptr = foldrM go 0 [0..3]
  where
    peek8le :: Int -> IO Word32
    peek8le = peekByteOff ptr

    go x acc = do
      bytes <- peek8le x :: IO Word32
      return $ acc .|. (bytes `shiftL` (x * 8))
{-# INLINE peek32le #-}

peek64le :: Ptr Word8 -> IO Word64
peek64le ptr = foldrM go 0 [0..7]
  where
    peek8le :: Int -> IO Word64
    peek8le = peekByteOff ptr

    go x acc = do
      bytes <- peek8le x
      return $ acc .|. (bytes `shiftL` (x * 8))
{-# INLINE peek64le #-}

prime1 :: Word64
prime1 = 0x9E3779B185EBCA87

-- XXH3_64bits_internal
xxh364 :: B.ByteString -> Word64 -> IO Word64
xxh364 bs@(B.PS fp@(ForeignPtr ptr x) (I# offset) (I# len)) seed
  | isTrue# (len <=# 3#) = hash_len_1to3
  | isTrue# (len <=# 8#) = hash_len_4to8
  | isTrue# (len <=# 16#) = hash_len_9to16
  | isTrue# (len <=# 128#) = hash_len_17to128
  | isTrue# (len <=# 240#) = hash_len_129to240
  | otherwise = hash_long
  where
    hash_len_1to3 = do
      let c1 = indexInt8OffAddr# ptr 0#
      let c2 = indexInt8OffAddr# ptr (len `iShiftRL#` 1#)
      let c3 = indexInt8OffAddr# ptr (len -# 1#)

      let combined = (I# c1 `shiftL` 16) .|.
                     (I# c2 `shiftL` 24) .|.
                     (I# c3 `shiftL`  0) .|.
                     (I# len `shiftL` 8)

      secret32 <- secret32WithOffset 0
      secret32Plus4 <- secret32WithOffset 4

      let bitflip = fromIntegral (secret32 `xor` secret32Plus4) + seed
      let keyed = fromIntegral combined `xor` fromIntegral bitflip
      return $ xxhash64Avalanche keyed


    hash_len_4to8 = do
      let seed' = seed `xor` fromIntegral (byteSwap32 (fromIntegral seed))
      
      let ptrWord = unsafeForeignPtrToPtr fp
      input1 <- peek32le ptrWord
      input2 <- peek32le (ptrWord `plusPtr` I# (len -# 4#))

      secret64Plus8 <- secret64WithOffset 8
      secret64Plus16 <- secret64WithOffset 16
      let bitflip = (secret64Plus8 `xor` secret64Plus16) - seed'
      let input64 = fromIntegral input2 + (fromIntegral input1 `shiftL` 32)
      let keyed = input64 `xor` bitflip
      return $ xxhash3_rrmxmx keyed (fromIntegral (I# len))


    hash_len_9to16 = do
      secret64Plus24 <- secret64WithOffset 24
      secret64Plus32 <- secret64WithOffset 32
      secret64Plus40 <- secret64WithOffset 40
      secret64Plus48 <- secret64WithOffset 48

      let bitflip1 = (secret64Plus24 `xor` secret64Plus32) + seed
      let bitflip2 = (secret64Plus40 `xor` secret64Plus48) - seed

      let ptrWord = unsafeForeignPtrToPtr fp
      inputLo <- peek64le ptrWord
      inputHi <- peek64le (ptrWord `plusPtr` I# (len -# 8#))

      let inputLo' = inputLo `xor` bitflip1
      let inputHi' = inputHi `xor` bitflip2
      let acc = fromIntegral (I# len) + byteSwap64 inputLo' + inputHi' + mul128_fold64 inputLo' inputHi'
      return $ xxhash3Avalanche acc

    -- For mid range keys, XXH3 uses a Mum-hash variant
    hash_len_17to128 = xxhash3Avalanche <$> foldrM go accum [0,32..(min (I# len) 96)]
      where
        accum = fromIntegral (I# len) * prime1
        ptrWord = unsafeForeignPtrToPtr fp

        go x acc = do
          secret <- kSecret
          ptrSecret <- withStorableArray secret $ \ptr -> return ptr

          let idx = x `quot` 32
          a <- xxhash3_mix16B (ptrWord `plusPtr` (x `quot` 2)) ptrSecret idx seed
          b <- xxhash3_mix16B (ptrWord `plusPtr` (I# len - ((idx + 1) * 16))) ptrSecret (idx + 16) seed

          return $ acc + a + b

    hash_len_129to240 = do
      secret <- kSecret -- redundant bro...
      ptrSecret <- withStorableArray secret $ \ptr -> return ptr

      firstRound <- foldrM go accum [0,1..7]
      secondRound <- foldrM roundsGo firstRound [8..numOfRounds]
      lastBytes <- xxhash3_mix16B (ptrWord `plusPtr` (I# len - 16)) ptrSecret (136 - 17) seed

      return . xxhash3Avalanche $ secondRound + lastBytes

      where
        accum = fromIntegral (I# len) * prime1
        ptrWord = unsafeForeignPtrToPtr fp
        numOfRounds = I# len `quot` 16

        go x acc = do
          secret <- kSecret -- redundant bro...
          ptrSecret <- withStorableArray secret $ \ptr -> return ptr
          a <- xxhash3_mix16B (ptrWord `plusPtr` (x * 16)) ptrSecret (x * 16) seed
          return $ acc + a

        roundsGo x acc = do
          secret <- kSecret -- redundant bro...
          ptrSecret <- withStorableArray secret $ \ptr -> return ptr
          a <- xxhash3_mix16B (ptrWord `plusPtr` (x * 16)) ptrSecret (16 * (x - 8) + 3) seed
          return $ acc + a

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

xxhash3Avalanche :: Word64 -> Word64
xxhash3Avalanche h64 = do
  let i1 = xorShift64 h64 37
  let i2 = i1 * 0x165667919E3779F9
  let i3 = xorShift64 i2 32
  i3

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

xxhash3_mix16B :: Ptr Word8 -> Ptr Word8 -> Int -> Word64 -> IO Word64
xxhash3_mix16B ptrWord ptrSecret secretOffset seed = do
  inputLo <- peek64le ptrWord
  inputHi <- peek64le (ptrWord `plusPtr` 8)

  secret1 <- secret64WithOffset secretOffset
  secret2 <- secret64WithOffset (secretOffset + 8)
  return $ mul128_fold64
    (inputLo `xor` (secret1 + seed))
    (inputHi `xor` (secret2 - seed))

xorShift64 :: Word64 -> Int -> Word64
xorShift64 v64 s = v64 `xor` (v64 `shiftR` s)
{-# INLINE xorShift64 #-}

-- | Does a 64-bit tp 128-bit multiply, then XOR folds it.
mul128_fold64 :: Word64 -> Word64 -> Word64
mul128_fold64 lhs rhs = let p = mul64To128 lhs rhs in word128Lo64 p `xor` word128Hi64 p

mul64To128 :: Word64 -> Word64 -> Word128
mul64To128 lhs rhs = fromInteger (toInteger lhs * toInteger rhs)

hashLongWithSeed :: Ptr Word8 -> Word64 -> Word64
hashLongWithSeed ptr = \case
  0 -> hashLong ptr (unsafePerformIO kSecret) -- I guess I can use unsafePerformIO here
  seed -> hashLong ptr (genSecretFromSeed seed)
  where
    genSecretFromSeed = undefined

hashLong :: Ptr Word8 -> StorableArray Int Word8 -> Word64
hashLong = undefined