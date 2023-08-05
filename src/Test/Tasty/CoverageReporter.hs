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

newtype ReportCoverage = MkReportCoverage   Bool
  deriving (Eq, Ord, Typeable)

instance IsOption ReportCoverage where
    defaultValue :: ReportCoverage
    defaultValue  = MkReportCoverage False
    parseValue = fmap MkReportCoverage . safeReadBool
    optionName = pure "report-coverage"
    optionHelp = pure "Generate per-test coverage data"
    optionCLParser = mkFlagCLParser (short 'c') (MkReportCoverage True)



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
        writeTix (name <> ".tix") tix
        putStrLn (show result)
        pure () 
        }

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