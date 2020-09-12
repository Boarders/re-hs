module Main where

import RE.StringSearch
import Data.ByteString as BS
import System.IO

main :: IO ()
main = do
  testFile
  withFile fileName ReadMode $ \hdlr -> do
    matchFromFile 8 hdlr pat
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

bstr :: ByteString
bstr =
  BS.replicate 100 1

testFile :: IO ()
testFile = do
  BS.writeFile fileName bstr
  
  
