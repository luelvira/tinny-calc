module OperationsSpec where

import Test.Hspec
import Calc.Operations
import Calc.Equation.Internal

spec :: Spec
spec = do
  describe "Test the operation function" $ do
    it "empty" $ shouldBe (operateWithPriority []) 0
    it "One Value" $ shouldBe (operateWithPriority [(ProcessValue $ NumericValue 5)]) 5
    it "Simple adition operation" $ shouldBe (operateWithPriority
                                              [ (ProcessValue $ NumericValue 5)
                                              , (ProcessOperation Sumatory)
                                              , (ProcessValue $ NumericValue 3)]) 8
    it "Simple difference operation" $ shouldBe (operateWithPriority
                                              [ (ProcessValue $ NumericValue 5)
                                              , (ProcessOperation Difference)
                                              , (ProcessValue $ NumericValue 3)]) 2
    it "Simple Multiplication operation" $ shouldBe (operateWithPriority
                                              [ (ProcessValue $ NumericValue 5)
                                              , (ProcessOperation Multiplication)
                                              , (ProcessValue $ NumericValue 3)]) 15
    it "Simple division operation" $ shouldBe (operateWithPriority
                                              [ (ProcessValue $ NumericValue 7)
                                              , (ProcessOperation Division)
                                              , (ProcessValue $ NumericValue 3)]) 2
    it "Combine addition and multiplication operation" $ shouldBe (operateWithPriority
                                              [ (ProcessValue $ NumericValue 7)
                                              , (ProcessOperation Sumatory)
                                              , (ProcessValue $ NumericValue 3)
                                              , (ProcessOperation Multiplication)
                                              , (ProcessValue $ NumericValue 10)
                                              ]) 37
    it "Combine difference and multiplication operation" $ shouldBe (operateWithPriority
                                              [ (ProcessValue $ NumericValue 7)
                                              , (ProcessOperation Difference)
                                              , (ProcessValue $ NumericValue 3)
                                              , (ProcessOperation Multiplication)
                                              , (ProcessValue $ NumericValue 10)
                                              ]) (-23)
    it "Combine high priority operations and multiplication operation" $ shouldBe (operateWithPriority
                                              [ (ProcessValue $ NumericValue 7)
                                              , (ProcessOperation Multiplication)
                                              , (ProcessValue $ NumericValue 3)
                                              , (ProcessOperation Division)
                                              , (ProcessValue $ NumericValue 10)
                                              ]) 2
