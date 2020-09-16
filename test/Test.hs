{-# LANGUAGE TypeApplications #-}

module Main where

import RE.StringSearch
import qualified Data.ByteString as BS
import System.IO
import Data.Foldable
import Data.Char
import Data.Primitive.ByteArray
import GHC.Word
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as Lazy



main :: IO ()
main = do
  testFile
  withFile fileName ReadMode $ \hdlr -> do
    matchFromFile (2^16) hdlr myPat >>= print
    pure ()

    
--  print $ match pat tgt


-- defaultMain  testSuite

{-
testSuite :: TestTree
testSuite = testGroup "Perfect Vector Sort"
  [ Immutable.testSuite
  , Mutable.testSuite
  ]
-}

fileName :: FilePath
fileName = "test.dat"

myPat :: ByteArray 
myPat = byteArrayFromList @Word8 [100,111,103]

bstr :: Lazy.ByteString
bstr =
  let
    notPat = foldMap Builder.word8 ([1,2,3,4,5])
    pat    = foldMap Builder.word8 ([100,111,103,112, 112])
    chunk  = fold (replicate (10 ^ 8) notPat)
  in
    Builder.toLazyByteString $
      chunk <> pat <> chunk


    --fold $ replicate (10^6) notPat <> [pat] <> replicate (10^5) notPat

testFile :: IO ()
testFile = do
  putStrLn "Writing file..."
  Lazy.writeFile fileName bstr
  putStrLn "File written"
  
  
