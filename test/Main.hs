module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "UnitTests" [
    testCase "testOne" (True @=? True),
    testCase "testTwo" (2 @=? (2 :: Int)),
    testCase "testThree" (2 @=? (3 :: Int))
    ]
