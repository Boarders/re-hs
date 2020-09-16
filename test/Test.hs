{-# LANGUAGE TypeApplications #-}

module Main where

import RE.StringSearch
import qualified Data.ByteString as BS
import System.IO
import Data.Foldable
import Data.Char
import Data.Primitive.ByteArray
import GHC.Word


main :: IO ()
main = do
 -- testFile
  withFile fileName ReadMode $ \hdlr -> do
    matchFromFile (2^10) hdlr myPat >>= print
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

bstr :: BS.ByteString
bstr =
  let
    notPat = BS.pack ([1,2,3,4,5])
    pat    = BS.pack ([100,111,103])
  in
    fold $ replicate (10^5) notPat <> [pat] <> replicate (10^5) notPat

testFile :: IO ()
testFile = do
  putStrLn "Writing file..."
  BS.writeFile fileName bstr
  putStrLn "File written"
  
  
