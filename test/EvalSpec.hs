{- |
Module: .home.lucas.Documents.git.dummy-calc.test.EvalSpec
Description: 
Copyright: (C) Lucas Elvira Martín <lucaselvira96@gmail.com>, 2024
License: GPL-3
Maintainer: lucaselvira96@gmail.com
-}
module EvalSpec where
import Test.Hspec

import DummyCalc.Eval
import DummyCalc.Lexer
import DummyCalc.Parser

import DummyCalc.Parser.Data.Internal
import DummyCalc.Lexer.Tokens

getRightExpression :: (Either ParErr Expr, [Token]) -> Expr
getRightExpression (Right r, []) = r

spec :: Spec
spec = do
  describe "Test the eval function" $ do
    it "Eval and parse val 5" $
      shouldBe (eval $ getRightExpression $ parseExpression $ lexer "5") 5
    it "Eval and parse value -5" $
      shouldBe (eval $ getRightExpression $ parseExpression $ lexer "-5") (-5)
    it "Eval and parse value 5+8" $
      shouldBe (eval $ getRightExpression $ parseExpression $ lexer "5+8") (13)
    it "Eval and parse value 5+8-3" $
      shouldBe (eval $ getRightExpression $ parseExpression $ lexer "5+8-3") (10)
    it "Eval and parse value 5*(9-8)" $
      shouldBe (eval $ getRightExpression $ parseExpression $ lexer "5*(9-8)") (5)
    it "Eval and parse value 5+(8*9)-3" $
      shouldBe (eval $ getRightExpression $ parseExpression $ lexer "5+(8*9)-3") (74)
    it "Eval and parse value 5*(14-8)/3+2" $
      shouldBe (eval $ getRightExpression $ parseExpression $ lexer "5*(14-8)/3+2") (12)
    it "Eval and parse value 5*(9-8)/3+2" $
      shouldBe (eval $ getRightExpression $ parseExpression $ lexer "5*(9-8)/3+2") (5/3 + 2)
  
