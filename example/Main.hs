module Main (main) where

import Test.Tasty
import Test.Tasty.CoverageReporter
import Test.Tasty.HUnit

main :: IO ()
main = defaultMainWithIngredients [coverageReporter] tests

tests :: TestTree
tests =
  testGroup
    "UnitTests"
    [ testCase "testOne" (True @=? True),
      testCase "testTwo" (2 @=? (2 :: Int)),
      testCase "testThree" (2 @=? (3 :: Int))
    ]
