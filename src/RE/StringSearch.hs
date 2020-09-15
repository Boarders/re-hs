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
import Control.Monad.Trans.Class

import Pipes as Pipes
import Pipes.Core as Pipes
import qualified Pipes.Prelude as Pipes
import Control.Monad ((>=>))
import Data.Foldable (traverse_)

usf = unsafePerformIO
tr str a = usf $ putStrLn str >> print a >> putStrLn "" >> return a
tr' str a = a

my_sum :: Producer Match IO (Ptr Word8) -> IO (Match, Ptr Word8)
my_sum = Pipes.fold' (<>) mempty id


matchFromFile :: Int -> Handle -> ByteArray -> IO Match
matchFromFile bufSize fileHdlr baPat = do
    fileSize <- hFileSize fileHdlr
    let (div, rem) = quotRem fileSize (fromIntegral bufSize)
    let tgtSize = bufSize + 15    
    mba <- newPinnedByteArray tgtSize
    let mbaPtrStart   = mutableByteArrayContents mba
    let matchPtrStart = advancePtr mbaPtrStart 15
    (divMatches, divPtr) <- my_sum $
           readFromHandle (fromIntegral div) bufSize fileHdlr matchPtrStart
       >>~ matchWorkP 0 initialState baPat tgtSize
    liftIO $ putStrLn $ "divMatches: " <> show divMatches
    remRes <- do
      let
        remI :: Int
        remI = fromIntegral rem
      hGetBufSome fileHdlr divPtr remI
      let divPtrStr = advancePtr divPtr (- 16)
      liftIO $ putStrLn "end"
      liftIO $ traverse_ (\n -> readOffPtr divPtrStr n >>= print) [0..(remI + 15)]
      matchWork baPat (remI + 15) divPtrStr
    pure $ divMatches <> remRes
      
      
          


readFromHandle
  :: Int -> Int -> Handle -> Ptr Word8 -> Proxy X () (Ptr Word8) (Ptr Word8) IO (Ptr Word8)
readFromHandle numReads readSize hdlr currPtr = do
  go numReads currPtr
  where
    go :: Int -> Ptr Word8 -> Proxy X () (Ptr Word8) (Ptr Word8) IO (Ptr Word8)
    go 0 ptr = do
      ptr' <- respond ptr
      pure ptr'
    go n ptr = do
      ptr' <- respond ptr
      liftIO $ putStrLn $ "div = " <> show n
      liftIO $ hGetBufSome hdlr ptr' readSize
      liftIO $ traverse_ (\n -> readOffPtr ptr' n >>= print) [0..31]
      go (n - 1) ptr'


data MatchState = MatchState
  { mStart :: {-# UNPACK #-} !Int
  , mBits  :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)



initialState :: MatchState
initialState =
  MatchState 0 accStart

accStart :: Int
accStart = fromIntegral $ maxBound @Word16

   
matchWorkP
  :: Int
  -> MatchState
  -> ByteArray
  -> Int
  -> Ptr Word8
  -> Proxy (Ptr Word8) (Ptr Word8) () Match IO (Ptr Word8)
matchWorkP c MatchState{..} baPat tgtS currPtr  = do
 -- liftIO $ print c
  tgtPtr <- request currPtr
  let I# mBits# = mBits
  (matchState, count) <- liftIO $ match_p 0 tgtPtr mStart mBits# 0
  let matches = Match count []
  let endPtr = advancePtr tgtPtr (tgtS - 15)  
  let newPtr = advancePtr tgtPtr 16
  liftIO $ copyPtr tgtPtr endPtr 15
  yield matches
  matchWorkP (c + 1) matchState baPat tgtS newPtr
  where
  I# accStart# = fromIntegral $ maxBound @Word16
  patS   = sizeofByteArray baPat
  endS   = 15 + patS
  hittingPt =
    let r = tgtS - endS + 1 in
      --print r >> return
      r
  match_p :: Int ->  Ptr Word8 -> Int -> Int# -> Int -> IO (MatchState, Int)
  -- There can be no more matches
  match_p i ptr j acc# count | i >= hittingPt = do
                               putStrLn "no match"
                               putStrLn $ "i = " <> show i
                               pure  $ (MatchState j (I# acc#), count)
  -- The current collection of considered matches don't work so skip forward by 16
  match_p i ptr j acc# count | tagToEnum# (acc# ==# 0#) =
    match_p (i + 16) (advancePtr ptr 16) 0 accStart# count
  -- A match is found
  match_p i ptr j acc# count | j == patS =
    let
      count' = (I# (word2Int# (popCnt# (int2Word# acc#)))) + count
    in
      do
        putStrLn ""        
        putStrLn "found"
        putStrLn $ "i = " <> show i
        match_p (i + 16) (advancePtr ptr 16) 0 accStart# count'
  -- Do work
  match_p i ptr j acc# count =
    let
      W8# p_val# = indexByteArray baPat j
      p_j16#  = broadcastWord8X16# p_val#
      j_comp# = simd_comp p_j16# ptr
      acc'#   = andI# acc# j_comp#
      ptr'    = advancePtr ptr 1
    in
      do
        putStrLn ""        
        putStrLn "work"
        putStrLn $ "i = " <> show i
        match_p (i + 1) (advancePtr ptr 1) (j + 1) acc'# count


data Match = Match
  { numMatch  :: {-# UNPACK #-} !Int
  , matchInds :: [Int]
  }
  deriving (Eq, Show)

instance Semigroup Match where
  (Match nm1 mInds1) <> (Match nm2 mInds2) = Match (nm1 + nm2) (mInds1 <> mInds2)


instance Monoid Match where
  mempty = Match 0 []
  
  
  

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

{-
match :: ByteArray -> Int -> MutableByteArray RealWorld -> Int
match baPat tgtS mbaTgt =
  let
    tgtPtr = mutableByteArrayContents  mbaTgt
  in
    matchWork baPat gtgtS tgtPtr
    

data Match = Match
  { numMatch  :: !Int
  , matchInds :: [Int]
  }

-}

matchWork
  :: ByteArray
  -> Int
  -> Ptr Word8
  -> IO Match
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
  finalPtr    = advancePtr tgtPtr tgtS
  

  endPtr = advancePtr tgtPtr (tgtS - endS)

  match_end :: Int -> Int -> Ptr Word8 -> Int -> IO Match
  match_end i j ptr count | i + (patS - (j + 1)) > endS =
    do
      putStrLn $ "first"
      putStrLn $ "j = " <> show j
      putStrLn $ "i = " <> show i
      pure $ Match count []
  match_end i j ptr count |  i >= endS =
    do
      putStrLn $ "j = " <> show j
      putStrLn $ "i = " <> show i
      pure $ Match count []
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
        do
        putStrLn $ "tgtV = " <> show tgtV
        match_end i j' ptr count
      else
        do
        putStrLn $ "tgtV = " <> show tgtV        
        match_end i' 0 (advancePtr ptr 1) count    
    
  
  match_p :: Int ->  Ptr Word8 -> Int -> Int# -> Int -> IO Match
  -- There can be no more matches
  match_p i ptr j acc# count | i >= hittingPt =
    do
      putStrLn ""
      putStrLn "endPtr"
      traverse_ (\n -> readOffPtr endPtr n >>= print) [0..19]
      (<> Match count []) <$> match_end 0 0 endPtr 0
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
      match_p (i + 1) (advancePtr ptr 1) (j + 1) acc'# count

  
--}
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
      


