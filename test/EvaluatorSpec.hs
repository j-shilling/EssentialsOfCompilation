module EvaluatorSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import AST
import Evaluator

spec :: Spec
spec = do
  describe "evaluate" $ do
    prop "integers evaluate to themselves" $ do
      \x -> evaluate (Int x) `shouldBe` Int x
