module Main where

import RE.StringSearch
import qualified Data.ByteString as BS
import System.IO
import Data.Foldable


main :: IO ()
main = do
  testFile
  withFile fileName ReadMode $ \hdlr -> do
    matchFromFile (2^5) hdlr pat >>= print
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



bstr :: BS.ByteString
bstr =
  let pat = BS.pack ([1,2,3,4,5])
  in fold (replicate 23 pat)

testFile :: IO ()
testFile = do
  putStrLn "Writing file..."
  BS.writeFile fileName bstr
  putStrLn "File written"
  
  
