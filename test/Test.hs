module Main where

import RE.StringSearch

main :: IO ()
main = do
  example >>= print
  print "eloo"


-- defaultMain  testSuite

{-
testSuite :: TestTree
testSuite = testGroup "Perfect Vector Sort"
  [ Immutable.testSuite
  , Mutable.testSuite
  ]
-}
