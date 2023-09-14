module Main (main) where

import System.FilePath
import System.Process (callCommand)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsFileDiff)

goldenDir :: FilePath
goldenDir = "test" </> "golden"

tixDir :: FilePath
tixDir = "tix"

main :: IO ()
main = defaultMain tests

-- | The build-tool-depends line in the cabal file should guarantee that the executable is available
runExample :: IO ()
runExample = callCommand "tasty-coverage-example --report-coverage --remove-tix-hash"

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
test1 = goldenVsFileDiff "testOne" diffCmd (goldenDir </> example <.> "golden") (tixDir </> example) runExample
  where
    example = "UnitTests.testOne.PASSED.tix"

test2 :: TestTree
test2 = goldenVsFileDiff "testTwo" diffCmd (goldenDir </> example <.> "golden") (tixDir </> example) runExample
  where
    example = "UnitTests.testTwo.PASSED.tix"

test3 :: TestTree
test3 = goldenVsFileDiff "testThree" diffCmd (goldenDir </> example <.> "golden") (tixDir </> example) runExample
  where
    example = "UnitTests.testThree.FAILED.tix"

test4 :: TestTree
test4 = goldenVsFileDiff "testFour" diffCmd (goldenDir </> example <.> "golden") (tixDir </> example) runExample
  where
    example = "UnitTests.testFour.FAILED.tix"

test5a :: TestTree
test5a = goldenVsFileDiff "testFive" diffCmd (goldenDir </> example <.> "golden") (tixDir </> example) runExample
  where
    example = "UnitTests.testFive.PASSED.tix"

test5b :: TestTree
test5b = goldenVsFileDiff "testFive" diffCmd (goldenDir </> example <.> "golden") (tixDir </> example) runExample
  where
    example = "UnitTests.testFive'.PASSED.tix"

diffCmd :: FilePath -> FilePath -> [String]
diffCmd ref new = ["diff", ref, new]
