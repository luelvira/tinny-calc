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
import DummyCalc.Environment

import qualified DummyCalc.Language as La

getRightExpression :: (Either ParErr Expr, [Token]) -> Expr
getRightExpression (Right r, _) = r
getRightExpression (Left l, _) = error $ show l

spec :: Spec
spec = do
  describe "Test the eval function" $ do
    it "Eval and parse val 5" $
      shouldBe (eval (getRightExpression $ parseExpression $ lexer "5") newNumEnvironment ) 5
    it "Eval and parse value -5" $
      shouldBe (eval  (getRightExpression $ parseExpression $ lexer "-5") newNumEnvironment) (-5)
    it "Eval and parse value 5+8" $
      shouldBe (eval (getRightExpression $ parseExpression $ lexer "5+8") newNumEnvironment) (13)
    it "Eval and parse value 5+8-3" $
      shouldBe (eval (getRightExpression $ parseExpression $ lexer "5+8-3") newNumEnvironment) (10)
    it "Eval and parse value 5*(9-8)" $
      shouldBe (eval (getRightExpression $ parseExpression $ lexer "5*(9-8)") newNumEnvironment) (5)
    it "Eval and parse value 5+(8*9)-3" $
      shouldBe (eval (getRightExpression $ parseExpression $ lexer "5+(8*9)-3") newNumEnvironment) (74)
    it "Eval and parse value 5*(14-8)/3+2" $
      shouldBe (eval (getRightExpression $ parseExpression $ lexer "5*(14-8)/3+2") newNumEnvironment) (12)
    it "Eval and parse value 5*(9-8)/3+2" $
      shouldBe (eval (getRightExpression $ parseExpression $ lexer "5*(9-8)/3+2") newNumEnvironment) (5/3 + 2)
    it "Eval and parse value x*(9-8)/3+2" $
      let nEnv = setNumValue "x" (La.NumValue 5) newNumEnvironment
      in
        shouldBe (eval (getRightExpression $ parseExpression $ lexer "x*(9-8)/3+2") nEnv) (5/3 + 2)
    it "Eval and parse value (x + y) * (2 + z) where x = 5, y = 9, z = 55" $
      shouldBe (eval
                (getRightExpression $ parseExpression $ lexer "(x + y) * (2 + z)") 
                (setNumValue "x" 5 $ setNumValue "y" 9 $ setNumValue "z" 55 newNumEnvironment)
               )
               ((5+9)*(2+55))
  
