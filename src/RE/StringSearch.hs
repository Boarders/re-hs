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

accursedUnutterablePerformIO :: IO a -> a
accursedUnutterablePerformIO (IO m) = case m realWorld# of (# _, r #) -> r


foreign import ccall unsafe "simd_comp" simd_comp :: Word8X16# -> Ptr Word8 -> Int#

example :: IO Int
example = do
  ptr <- array
  return $ I# (simd_comp a# ptr)
  where
    a# = packWord8X16#
           (# 1##  , 2## , 3## , 4##
            , 1##  , 2## , 3## , 4##
            , 1##  , 2## , 3## , 4##
            , 1##  , 2## , 3## , 4##
            #)

array :: IO (Ptr Word8)
array = do
  mba <- unsafeThawByteArray ba
  return (mutableByteArrayContents mba)
  where
  ba = byteArrayFromList @Word8 (take 16 $ repeat 1)


match :: ByteArray -> ByteArray -> Int
match baPat baTgt =
  let
    tgtPtr = byteArrayContents baTgt
    tgtS   = sizeofByteArray baTgt
  in
    matchWork baPat tgtS tgtPtr
    

data Match
  { numMatch  :: !Int
  , matchInds :: [Int]
  }



matchWork
  :: ByteArray
  -> Int
  -> Ptr Word8
  -> Int
matchWork mbPat tgtS tgtPtr = undefined
  where
  patS   = sizeofByteArray mbPat
  hittingPt = max (tgtS - 16) (tgtS - patS) + 1
  finalPtr  = advancePtr tgtPtr hittingPt
  
  match_p :: Ptr Word8 -> Int -> Int# -> Int -> Int
  -- There can be no more matches
  match_p ptr j acc# count | ptr == finalPtr = count
  -- The current collection of considered matches don't work so skip forward by 16
  match_p ptr j acc# count | tagToEnum# (acc# ==# 0#) =
    match_p (advancePtr ptr 16) 0 0# count
  -- A match is found
  match_p ptr j acc# count | j == patS =
    let
      count' = (I# (word2Int# (popCnt# (int2Word# acc#)))) + count
    in
      match_p (advancePtr ptr 16) 0 0# count'
  -- Do work
  match_p ptr j acc# count =
    let
      W8# p_val# = indexByteArray mbPat j
      p_j16# = broadcastWord8X16# p_val#
      j_comp# = simd_comp p_j16# tgtPtr
      acc'# = andI# acc# j_comp#
    in
      match_p (advancePtr ptr 1) (j + 1) acc'# count

  

