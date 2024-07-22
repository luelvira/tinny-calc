{- |
Module: test.LexerSpec
Description: Test DummyCalc.Lexer.hs
Copyright: (C) Lucas Elvira Martín <lucaselvira96@gmail.com>, 2024
License: GPL-3
Maintainer: lucaselvira96@gmail.com
-}
module LexerSpec where
import Test.Hspec

import qualified DummyCalc.Lexer as Lexer
import qualified DummyCalc.Lexer.Tokens as Token
import DummyCalc.Language.Data.Internal as La

spec :: Spec
spec = do
  describe "Test the lexer function" $ do
    it "Test (5+9)" $
      shouldBe (Lexer.lexer "(5+9)")
               [ Token.TokLeftParen
               , Token.TokNumber $ La.Value 5
               , Token.TokOperator $ La.Summatory
               , Token.TokNumber $ La.Value 9
               , Token.TokRightParen
               ]
    it "Test 5 + 9" $
      shouldBe (Lexer.lexer "5 + 9")
               [ Token.TokNumber $ La.Value 5
               , Token.TokOperator $ La.Summatory
               , Token.TokNumber $ La.Value 9
               ]
    it "Test 5 + -9" $
      shouldBe (Lexer.lexer "5 + -9")
               [ Token.TokNumber $ La.Value 5
               , Token.TokOperator $ La.Summatory
               , Token.TokOperator $ La.Difference
               , Token.TokNumber $ La.Value 9
               ]
    it "Test 7*9/3" $
      shouldBe (Lexer.lexer "7*9/3")
               [ Token.TokNumber $ La.Value 7
               , Token.TokOperator $ La.Multiplication
               , Token.TokNumber $ La.Value 9
               , Token.TokOperator $ La.Division
               , Token.TokNumber $ La.Value 3
               ]
      
