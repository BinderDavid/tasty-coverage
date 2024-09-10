module Main (main) where

import Data.Maybe (fromJust)
import System.FilePath
import System.Process (callCommand)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import Trace.Hpc.Tix (Tix (..), TixModule (..), readTix)

goldenDir :: FilePath
goldenDir = "test" </> "golden"

tixDir :: FilePath
tixDir = "tix"

main :: IO ()
main = defaultMain tests

diffTixFiles :: String -> TestTree
diffTixFiles example = testCase example $ do
  -- The build-tool-depends line in the cabal file should guarantee that the executable is available
  callCommand "tasty-coverage-example --report-coverage --remove-tix-hash"
  let goldenFile = goldenDir </> example <.> "golden"
  let generatedFile = tixDir </> example
  goldenTix <- fromJust <$> readTix goldenFile
  generatedTix <- fromJust <$> readTix generatedFile
  if tixEq goldenTix generatedTix
    then pure ()
    else assertFailure "Generated and golden tix file are not equal"

-- | A comparison function which ignores irrelevant information that might change
-- between ghc/cabal versions. This is necessary to use golden files which work for
-- multiple versions.
tixEq :: Tix -> Tix -> Bool
tixEq (Tix mods1) (Tix mods2) = and (zipWith (\m1 m2 -> tixModEq m1 m2) mods1 mods2)
  where
    tixModEq :: TixModule -> TixModule -> Bool
    tixModEq (TixModule _ _ i1 j1) (TixModule _ _ i2 j2) = i1 == i2 && j1 == j2

tests :: TestTree
tests =
  testGroup
    "GoldenTests"
    [ test1,
      test2,
      test3,
      test4,
      test5a,
      test5b
    ]

test1 :: TestTree
test1 = diffTixFiles "UnitTests.testOne.PASSED.tix"

test2 :: TestTree
test2 = diffTixFiles "UnitTests.testTwo.PASSED.tix"

test3 :: TestTree
test3 = diffTixFiles "UnitTests.testThree.FAILED.tix"

test4 :: TestTree
test4 = diffTixFiles "UnitTests.testFour.FAILED.tix"

test5a :: TestTree
test5a = diffTixFiles "UnitTests.testFive.PASSED.tix"

test5b :: TestTree
test5b = diffTixFiles "UnitTests.testFive'.PASSED.tix"
