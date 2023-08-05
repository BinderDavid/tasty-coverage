{-# LANGUAGE InstanceSigs #-}
module Test.Tasty.CoverageReporter (coverageReporter) where

import Test.Tasty
import Test.Tasty.Ingredients
import Test.Tasty.Options
import Test.Tasty.Runners
import Data.Typeable
import Options.Applicative

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
testNames :: OptionSet -> TestTree -> [TestName]
testNames {- opts -} {- tree -} =
  foldTestTree
    trivialFold
      { foldSingle = \_opts name _test -> [name]
      , foldGroup = \_opts groupName names -> map ((groupName ++ ".") ++) names
      }

coverageReporter :: Ingredient
coverageReporter = TestManager coverageOptions coverageRunner

coverageOptions :: [OptionDescription]
coverageOptions = [Option (Proxy :: Proxy ReportCoverage)]

coverageRunner :: OptionSet -> TestTree -> Maybe (IO Bool)
coverageRunner opts tree = case lookupOption opts of
  MkReportCoverage False -> Nothing
  MkReportCoverage True -> Just $ do
    mapM_ putStrLn $ testNames opts tree
    return True