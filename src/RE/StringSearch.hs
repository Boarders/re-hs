{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE UnliftedFFITypes         #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE BangPatterns             #-}

module RE.StringSearch where


import GHC.Prim

import Data.Primitive.ByteArray
import Data.Primitive.Ptr
import Data.Primitive.Types
import GHC.Word
import GHC.Int
import Data.Bits
import GHC.Types (IO(..))
import System.IO.Unsafe (unsafePerformIO)
-- import Data.ByteString
import System.IO

usf = unsafePerformIO
tr str a = usf $ putStrLn str >> print a >> putStrLn "" >> return a
tr' str a = a


matchFromFile :: Int -> Handle -> ByteArray -> IO Int
matchFromFile bufSize fileHdlr baPat = do
    fileSize <- hFileSize fileHdlr
    let (div, rem) = quotRem fileSize (fromIntegral bufSize)
    let fullBuf = buf bufSize
    let lastBuf = buf (fromIntegral rem)
    print div
    print rem
    return 1
  where

    buf :: Int -> IO Int
    buf bsz  = do
      mba <- newPinnedByteArray bsz
      let mbaPtr = mutableByteArrayContents mba
      hGetBufSome fileHdlr mbaPtr bsz


  
  
  

accursedUnutterablePerformIO :: IO a -> a
accursedUnutterablePerformIO (IO m) = case m realWorld# of (# _, r #) -> r


foreign import ccall unsafe "simd_comp" simd_comp :: Word8X16# -> Ptr Word8 -> Int#

example :: Int
example = I# (simd_comp a# array)
  where
    a# = packWord8X16#
           (# 1##  , 2## , 3## , 4##
            , 1##  , 2## , 3## , 4##
            , 1##  , 2## , 3## , 4##
            , 1##  , 2## , 3## , 4##
            #)

array :: Ptr Word8
array = byteArrayContents ba
  where
  ba = byteArrayFromList @Word8 (take 16 $ repeat 1)


pat :: ByteArray
pat = byteArrayFromList @Word8 [1,2,3,4]

tgt :: ByteArray
tgt = byteArrayFromList @Word8
  ([1,2,3,4] <> [1,2,3,4] <> (replicate 25 1) <> [1,2,3,4] <> (replicate 25 1) <> [1,2,3,4,8,1,2,3,1,2,3,4,2,3,4])


match :: ByteArray -> Int -> MutableByteArray RealWorld -> Int
match baPat tgtS mbaTgt =
  let
    tgtPtr = mutableByteArrayContents  mbaTgt
  in
    matchWork baPat tgtS tgtPtr
    

data Match = Match
  { numMatch  :: !Int
  , matchInds :: [Int]
  }



matchWork
  :: ByteArray
  -> Int
  -> Ptr Word8
  -> Int
matchWork mbPat tgtS tgtPtr = match_p 0 tgtPtr 0 accStart# 0
  where
  I# accStart# = fromIntegral $ maxBound @Word16
  patS   = sizeofByteArray mbPat
  endS   = 15 + patS
  hittingPt =
    let r = tgtS - endS + 1 in
      --print r >> return
      r
  finalPtr_p  = advancePtr tgtPtr hittingPt
  finalPtr    = advancePtr tgtPtr (tgtS - patS + 1)
  

  endPtr = advancePtr tgtPtr (tgtS - endS)

  match_end :: Int -> Int -> Ptr Word8 -> Int -> Int
  match_end i j ptr count | ptr == finalPtr = count
  match_end i j ptr count | j == patS =
    let
      i' = (i + 1)
    in
      match_end i' 0 (advancePtr ptr 1) (count + 1)
  match_end i j ptr count =
    let
      patV = indexByteArray mbPat j
      tgtV = indexOffPtr ptr j
      i' = (i + 1)
      j' = (j + 1)
    in
      if patV == tgtV then
        match_end i j' ptr count
      else
        match_end i' 0 (advancePtr ptr 1) count
    
    
  
  match_p :: Int ->  Ptr Word8 -> Int -> Int# -> Int -> Int
  -- There can be no more matches
  match_p i ptr j acc# count | i >= hittingPt =
                               --tr "1" $
                               count + match_end 0 0 endPtr 0
  -- The current collection of considered matches don't work so skip forward by 16
  match_p i ptr j acc# count | tagToEnum# (acc# ==# 0#) = --tr "2" $
    match_p (i + 16) (advancePtr ptr 16) 0 accStart# count
  -- A match is found
  match_p i ptr j acc# count | j == patS =
    let
      count' = (I# (word2Int# (popCnt# (int2Word# acc#)))) + count
    in
--      tr "3" $ 
      match_p (i + 16) (advancePtr ptr 16) 0 accStart# count'
  -- Do work
  match_p i ptr j acc# count =
    let
      W8# p_val# = indexByteArray mbPat j
      p_j16#  = broadcastWord8X16# p_val#
      j_comp# = simd_comp p_j16# ptr
      acc'#   = andI# acc# j_comp#
      ptr'    = advancePtr ptr 1
    in
--      tr "4" $ 
      match_p (i + 1) (advancePtr ptr 1) (j + 1) acc'# count

  

getBitsSet :: Int -> Int -> [Int]
getBitsSet off = go []
  where
  go :: [Int] -> Int -> [Int]
  go acc 0    = acc
  go acc bits =
    let
      leadBit = countTrailingZeros bits
      bits'   = clearBit leadBit bits
      ind     = off + (16 - leadBit)
    in
      go ((leadBit + off):acc) bits'
      


