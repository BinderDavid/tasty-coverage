{-# LANGUAGE CPP #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{- |
Module         : Test.Tasty.CoverageReporter
Description    : Ingredient for producing per-test coverage reports

This module provides an ingredient for the tasty framework which allows
to generate one coverage file per individual test.
-}
module Test.Tasty.CoverageReporter (coverageReporter) where

import Control.Monad (forM_)
import Data.Bifunctor (first)
import Data.Foldable (fold)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Typeable
import System.FilePath ((<.>), (</>))
import Test.Tasty
import Test.Tasty.Ingredients
import Test.Tasty.Options
import Test.Tasty.Providers
import Test.Tasty.Runners
import Trace.Hpc.Reflect (clearTix, examineTix)
import Trace.Hpc.Tix (Tix (..), TixModule (..), writeTix)

-------------------------------------------------------------------------------
-- Options
-------------------------------------------------------------------------------

newtype ReportCoverage = MkReportCoverage Bool
  deriving (Eq, Ord, Typeable)

instance IsOption ReportCoverage where
  defaultValue = MkReportCoverage False
  parseValue = fmap MkReportCoverage . safeReadBool
  optionName = pure "report-coverage"
  optionHelp = pure "Generate per-test coverage data"
  optionCLParser = mkFlagCLParser mempty (MkReportCoverage True)

newtype RemoveTixHash = MkRemoveTixHash Bool
  deriving (Eq, Ord, Typeable)

instance IsOption RemoveTixHash where
  defaultValue = MkRemoveTixHash False
  parseValue = fmap MkRemoveTixHash . safeReadBool
  optionName = pure "remove-tix-hash"
  optionHelp = pure "Remove hash from tix file (used for golden tests)"
  optionCLParser = mkFlagCLParser mempty (MkRemoveTixHash True)

coverageOptions :: [OptionDescription]
coverageOptions =
  [ Option (Proxy :: Proxy ReportCoverage),
    Option (Proxy :: Proxy RemoveTixHash)
  ]

-------------------------------------------------------------------------------
-- coverageReporter Ingredient
-------------------------------------------------------------------------------

tixDir :: FilePath
tixDir = "tix"

-- | Obtain the list of all tests in the suite
testNames :: OptionSet -> TestTree -> IO ()
testNames os tree = forM_ (foldTestTree coverageFold os tree) $ \(s, f) -> f (fold (NE.intersperse "." s))

type FoldResult = [(NonEmpty TestName, String -> IO ())]

#if MIN_VERSION_tasty(1,5,0)
groupFold :: OptionSet -> TestName -> [FoldResult] -> FoldResult
groupFold _ groupName acc = fmap (first (NE.cons groupName)) (concat acc)
#else
groupFold :: OptionSet -> TestName -> FoldResult -> FoldResult
groupFold _ groupName acc = fmap (first (NE.cons groupName)) acc
#endif

-- | Collect all tests and
coverageFold :: TreeFold FoldResult
coverageFold =
  trivialFold
    { foldSingle = \opts name test -> do
        let f n = do
              -- Collect the coverage data for exactly this test.
              clearTix
              result <- run opts test (\_ -> pure ())
              tix <- examineTix
              let filepath = tixFilePath n result
              writeTix filepath (removeHash opts tix)
              putStrLn ("Wrote coverage file: " <> filepath)
        pure (NE.singleton name, f),
      -- Append the name of the testgroup to the list of TestNames
      foldGroup = groupFold
    }

tixFilePath :: TestName -> Result -> FilePath
tixFilePath tn Result {resultOutcome} =
  tixDir </> generateValidFilepath tn <.> outcomeSuffix resultOutcome <.> ".tix"

-- | We want to compute the file suffix that we use to distinguish
-- tix files for failing and succeeding tests.
outcomeSuffix :: Outcome -> String
outcomeSuffix Success = "PASSED"
outcomeSuffix (Failure TestFailed) = "FAILED"
outcomeSuffix (Failure (TestThrewException _)) = "EXCEPTION"
outcomeSuffix (Failure (TestTimedOut _)) = "TIMEOUT"
outcomeSuffix (Failure TestDepFailed) = "SKIPPED"

-- | This ingredient implements its own test-runner which can be executed with
-- the @--report-coverage@ command line option.
-- The testrunner executes the tests sequentially and emits one coverage file
-- per executed test.
-- 
-- @since 0.1.0.0
coverageReporter :: Ingredient
coverageReporter = TestManager coverageOptions coverageRunner

coverageRunner :: OptionSet -> TestTree -> Maybe (IO Bool)
coverageRunner opts tree = case lookupOption opts of
  MkReportCoverage False -> Nothing
  MkReportCoverage True -> Just $ do
    testNames opts tree
    pure True

-- | Removes all path separators from the input String in order
-- to generate a valid filepath.
-- The names of some tests contain path separators, so we have to
-- remove them.
generateValidFilepath :: String -> FilePath
generateValidFilepath = filter (`notElem` pathSeparators)
  where
    -- Include both Windows and Posix, so that generated .tix files
    -- are consistent among systems.
    pathSeparators = ['\\', '/']

removeHash :: OptionSet -> Tix -> Tix
removeHash opts (Tix txs) = case lookupOption opts of
  MkRemoveTixHash False -> Tix txs
  MkRemoveTixHash True -> Tix (fmap removeHashModule txs)

removeHashModule :: TixModule -> TixModule
removeHashModule (TixModule name _hash i is) = TixModule name 0 i is
