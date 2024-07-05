module CalcEquationSpec where

import Test.Hspec
-- import Data.Typeable
import Data.Maybe (isNothing, fromJust)

import Calc.Equation.Internal


spec :: Spec
spec = do
  -- parse

  -- parseEquation
  describe "Test the parseEquation" $ do
    it "empty" $ case parseEquation Nothing [] of
                   Right _ -> return ()
                   _ -> expectationFailure "Expected Right size"
    it "Simple value" $ let
      result = case parseEquation Nothing ["99"] of
        Right r -> r
        _ -> []
      in
        shouldBe result [ProcessValue $ NumericValue 99]
    it "Two value" $ let
      result = case parseEquation Nothing ["99", "+", "9"] of
        Right r -> r
        _ -> []
      in
        shouldBe result [
            ProcessValue $ NumericValue 99
          , ProcessOperation Sumatory
          , ProcessValue $ NumericValue 9]
    it "Two value with negative number" $ let
      result = case parseEquation Nothing ["99", "+", "-9"] of
        Right r -> r
        _ -> []
      in
        shouldBe result [
            ProcessValue $ NumericValue 99
            , ProcessOperation Sumatory
            , ProcessValue $ NumericValue (-9)]

    it "Three values with negative number and letter" $
      let result = parseEquation Nothing ["p", "99", "+", "-9", "*", "p"]
      in
        case result of
          Left _ -> return ()
          _ -> expectationFailure "Expected Left MyExceptions"
  -- isNumber
  describe "Check if string is Number" $ do
    it "empty" $ shouldBe (isNumber "") False
    it "invalid" $ shouldBe (isNumber "abc") False
    it "valid Int" $ shouldBe (isNumber "9") True
    it "valid Large Number" $ shouldBe (isNumber "9999") True
    it "valid Negative" $ shouldBe (isNumber " -9") True
    it "Invalid Float" $ shouldBe (isNumber "9.89") False

  -- isValid

  describe "Check if equation is valid" $ do
    it "empty" $ let
      equation = case parseEquation Nothing [] of
                   Right r -> r
      in
        shouldBe (isValid equation) True
    it "Unique value" $ let
        equation = [ProcessValue $ NumericValue 99]
        in
          shouldBe (isValid equation) True
    it "Invalid value" $ let
        equation = [
            ProcessValue $ NumericValue 99
          , ProcessOperation Sumatory
          , ProcessValue $ NumericValue 9
          , ProcessOperation Sumatory]
        in
          shouldBe (isValid equation) False
    it "Invalid value start with operation" $ let
        equation = [
            ProcessOperation Sumatory
          , ProcessValue $ NumericValue 99
          , ProcessOperation Sumatory
          , ProcessValue $ NumericValue 9]
        in
          shouldBe (isValid equation) False
    it "Valid value" $ let
        equation = [
            ProcessValue $ NumericValue 99
          , ProcessOperation Sumatory
          , ProcessValue $ NumericValue 1]
        in
          shouldBe (isValid equation) True
    it "Valid value with multiple operation" $ let
        equation = [
            ProcessValue $ NumericValue 99
          , ProcessOperation Sumatory
          , ProcessValue $ NumericValue 1
          , ProcessOperation Difference
          , ProcessValue $ NumericValue 9]
        in
          shouldBe (isValid equation) True
    it "Valid value with negative values" $ let
        equation = [
            ProcessValue $ NumericValue 99
          , ProcessOperation Sumatory
          , ProcessValue $ NumericValue (-9)]
        in
          shouldBe (isValid equation) True
  describe "Check isValid funciton with invalid order" $ do
    it "Multiple operation consecutives" $ let
        equation = case parseEquation Nothing ["99", "+", "-"] of
                     Right r -> r
        in
          shouldBe (isValid equation) False

    it "Multiple number consecutives" $ let
        equation = case parseEquation Nothing ["99", "+", "8", "-65"] of
                     Right r -> r
        in
          shouldBe (isValid equation) False
    it "Starts with operation" $ let
        equation = case parseEquation Nothing ["*", "99", "+", "8"] of
                     Right r -> r
        in
          shouldBe (isValid equation) False
    it "Ends with operation" $ let
        equation = case parseEquation Nothing ["99", "+", "8", "+"] of
                     Right r -> r
        in
          shouldBe (isValid equation) False

  -- isValue

  -- isOperation

  -- ConvertToInt
  describe "Test suit for the function convert to int" $ do
    it "empty" $ shouldBe (isNothing $ convertToInt "") True
    it "invalid" $ shouldBe (isNothing $ convertToInt "po") True
    it "add string at the end" $ shouldBe (isNothing $ convertToInt "95po") True
    it "add String at the beginning" $ shouldBe (isNothing $ convertToInt "po95") True
    it "Valid value" $ shouldBe (fromJust (convertToInt "85") == ProcessValue (NumericValue 85)) True
    it "Valid negative value" $ shouldBe (fromJust (convertToInt " -85") == ProcessValue (NumericValue (-85))) True

