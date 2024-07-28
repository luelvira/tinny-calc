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
import DummyCalc.Language as La

spec :: Spec
spec = do
  describe "Test the lexer function" $ do
    it "Test (5+9)" $
      shouldBe (Lexer.lexer "(5+9)")
               [ Token.TokLeftParen
               , Token.TokNumber $ La.NumValue 5
               , Token.TokOperator $ La.Summatory
               , Token.TokNumber $ La.NumValue 9
               , Token.TokRightParen
               ]
    it "Test 5 + 9" $
      shouldBe (Lexer.lexer "5 + 9")
               [ Token.TokNumber $ La.NumValue 5
               , Token.TokOperator $ La.Summatory
               , Token.TokNumber $ La.NumValue 9
               ]
    it "Test 5 + -9" $
      shouldBe (Lexer.lexer "5 + -9")
               [ Token.TokNumber $ La.NumValue 5
               , Token.TokOperator $ La.Summatory
               , Token.TokOperator $ La.Difference
               , Token.TokNumber $ La.NumValue 9
               ]
    it "Test 7*9/3" $
      shouldBe (Lexer.lexer "7*9/3")
               [ Token.TokNumber $ La.NumValue 7
               , Token.TokOperator $ La.Multiplication
               , Token.TokNumber $ La.NumValue 9
               , Token.TokOperator $ La.Division
               , Token.TokNumber $ La.NumValue 3
               ]
    it "Test let x = 5" $
      shouldBe
        (Lexer.lexer "x => 5")
        [ Token.TokVar $ La.Variable "x"
        , Token.TokEquals
        , Token.TokNumber $ La.NumValue 5
        ]
    it "Test x*9" $
      shouldBe
        ( Lexer.lexer "x*9")
        [ Token.TokVar $ La.Variable "x"
        , Token.TokOperator $ La.Multiplication
        , Token.TokNumber $ La.NumValue 9
        ]
    it "Test x*(9-8)/3+2" $
      shouldBe
        ( Lexer.lexer "x*(9-8)/3+2")
        [ Token.TokVar $ La.Variable "x"
        , Token.TokOperator La.Multiplication
        , Token.TokLeftParen
        , Token.TokNumber $ La.NumValue 9
        , Token.TokOperator La.Difference
        , Token.TokNumber $ La.NumValue 8
        , Token.TokRightParen
        , Token.TokOperator La.Division
        , Token.TokNumber $ La.NumValue 3
        , Token.TokOperator La.Summatory
        , Token.TokNumber $ La.NumValue 2
        ]
      
    
      
