module Main where

import RE.StringSearch

main :: IO ()
main = do
--  print example
  print $ match pat tgt


-- defaultMain  testSuite

{-
testSuite :: TestTree
testSuite = testGroup "Perfect Vector Sort"
  [ Immutable.testSuite
  , Mutable.testSuite
  ]
-}
