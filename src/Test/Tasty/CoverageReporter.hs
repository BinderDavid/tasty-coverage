{-# LANGUAGE InstanceSigs #-}
module Test.Tasty.CoverageReporter (coverageReporter) where

import Test.Tasty
import Test.Tasty.Ingredients
import Test.Tasty.Options
import Test.Tasty.Runners
import Test.Tasty.Providers
import Data.Typeable
import Options.Applicative
import Trace.Hpc.Reflect
import Trace.Hpc.Tix
import Data.List
import System.FilePath

newtype ReportCoverage = MkReportCoverage   Bool
  deriving (Eq, Ord, Typeable)

instance IsOption ReportCoverage where
    defaultValue :: ReportCoverage
    defaultValue  = MkReportCoverage False
    parseValue = fmap MkReportCoverage . safeReadBool
    optionName = pure "report-coverage"
    optionHelp = pure "Generate per-test coverage data"
    optionCLParser = mkFlagCLParser (short 'c') (MkReportCoverage True)


tixDir :: FilePath
tixDir = "tix"

-- | Obtain the list of all tests in the suite
testNames :: OptionSet -> TestTree -> IO ()
testNames {- opts -} {- tree -} =
  foldTestTree coverageFold

coverageFold :: TreeFold (IO ())
coverageFold = trivialFold
       { foldSingle = \opts name test -> do
        clearTix
        result <- run opts test (\_ -> pure ())
        tix <- examineTix
        let filepath = tixFilePath name (hasPassed result)
        writeTix filepath tix
        putStrLn (show result)
        pure () 
        }

tixFilePath :: TestName -> Bool -> FilePath
tixFilePath tn True  = tixDir </> tn <.> "PASSED" <.> ".tix"
tixFilePath tn False = tixDir </> tn <.> "FAILED" <.> ".tix"

coverageReporter :: Ingredient
coverageReporter = TestManager coverageOptions coverageRunner

coverageOptions :: [OptionDescription]
coverageOptions = [Option (Proxy :: Proxy ReportCoverage)]

coverageRunner :: OptionSet -> TestTree -> Maybe (IO Bool)
coverageRunner opts tree = case lookupOption opts of
  MkReportCoverage False -> Nothing
  MkReportCoverage True -> Just $ do
    testNames opts tree
    pure True

-- Hacky Hacky Hacky
hasPassed :: Result -> Bool
hasPassed res = "Result {resultOutcome = Success" `isPrefixOf` (show res)
  