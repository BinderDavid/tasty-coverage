module Test.Tasty.CoverageReporter (coverageReporter) where

import Test.Tasty
import Test.Tasty.Ingredients (Ingredient(..))
import Test.Tasty.Options (OptionDescription, OptionSet)

coverageReporter :: Ingredient
coverageReporter = TestManager coverageOptions coverageRunner

coverageOptions :: [OptionDescription]
coverageOptions = []

coverageRunner :: OptionSet -> TestTree -> Maybe (IO Bool)
coverageRunner _ _ = Nothing