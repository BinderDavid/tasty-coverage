{-# LANGUAGE InstanceSigs #-}
module Test.Tasty.CoverageReporter (coverageReporter) where

import Test.Tasty
import Test.Tasty.Ingredients
import Test.Tasty.Options
import Test.Tasty.Runners
import Test.Tasty.Providers
import Data.Typeable
import qualified Options.Applicative as P
import Trace.Hpc.Reflect ( clearTix, examineTix )
import Trace.Hpc.Tix ( writeTix )
import System.FilePath ( (<.>), (</>) )
import Control.Monad (forM_)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty ( NonEmpty )
import Data.Foldable (fold)
import Data.Bifunctor (first)

newtype ReportCoverage = MkReportCoverage   Bool
  deriving (Eq, Ord, Typeable)

instance IsOption ReportCoverage where
    defaultValue  = MkReportCoverage False
    parseValue = fmap MkReportCoverage . safeReadBool
    optionName = pure "report-coverage"
    optionHelp = pure "Generate per-test coverage data"
    optionCLParser = mkFlagCLParser (P.short 'c') (MkReportCoverage True)


tixDir :: FilePath
tixDir = "tix"

-- | Obtain the list of all tests in the suite
testNames :: OptionSet -> TestTree -> IO ()
testNames  os tree = forM_ (foldTestTree coverageFold os tree) $ \(s,f) -> f (fold (NE.intersperse "." s))



-- | Collect all tests and
coverageFold :: TreeFold [(NonEmpty TestName, String -> IO ())]
coverageFold = trivialFold
       { foldSingle = \opts name test -> do
          let f n = do
                -- Collect the coverage data for exactly this test.
                clearTix
                result <- run opts test (\_ -> pure ())
                tix <- examineTix
                let filepath = tixFilePath n result
                writeTix filepath tix
                putStrLn ("Wrote coverage file: " <> filepath)
          pure (NE.singleton name, f),
          -- Append the name of the testgroup to the list of TestNames
          foldGroup = \_ groupName acc -> fmap (first (NE.cons groupName)) acc
        }

tixFilePath :: TestName -> Result -> FilePath
tixFilePath tn Result { resultOutcome = Success }  = tixDir </> tn <.> "PASSED" <.> ".tix"
tixFilePath tn Result { resultOutcome = Failure _ } = tixDir </> tn <.> "FAILED" <.> ".tix"

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

  