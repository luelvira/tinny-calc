{- |
Module: .home.lucas.Documents.git.dummy-calc.test.ParserSpec
Description: Test the Parser module
Copyright: (C) Lucas Elvira Martín <lucaselvira96@gmail.com>, 2024
License: GPL-3
Maintainer: lucaselvira96@gmail.com


-}
module ParserSpec where
import Test.Hspec

import DummyCalc.Lexer
import DummyCalc.Lexer.Tokens
import DummyCalc.Parser
import DummyCalc.Parser.Data.Internal
import qualified DummyCalc.Language as La

spec :: Spec
spec = do
  describe "Parse some values" $ do
    it "Parse value 5" $
      shouldBe ( parseVal [TokNumber $ La.Value 5]) (Right $ Val $ La.Value 5, [])
    it "Parse value -5" $
      shouldBe ( parseVal [TokOperator La.Difference, TokNumber $ La.Value 5]) (Right $ Val $ La.Value (-5), [])
    it "Parse value 58 + 56" $
      shouldBe ( parseVal
                 [ TokNumber $ La.Value 58
                 , TokOperator La.Summatory
                 , TokNumber $ La.Value 56
                 ]
               ) (Right $ Val $ La.Value 58, [TokOperator La.Summatory , TokNumber $ La.Value 56])
  describe "Parse some expressions" $ do
    it "Parse 5+8" $
      shouldBe (parseExpression $ lexer "5+8") (Right $ Add (Val $ La.Value 5) (Val $ La.Value 8), [])
    it "Parse 5+8-3" $
      shouldBe (parseExpression $ lexer "5+8-3") (Right
                                                  (Sub
                                                   (Add
                                                    (Val $ La.Value 5) (Val $ La.Value 8)
                                                   )
                                                  (Val $ La.Value 3))
                                                  , [])
  it "Parse 5*(9-8)" $
    shouldBe (parseExpression $ lexer "5*(9-8)") (Right
                                                  (Mul (Val $ La.Value 5)
                                                    ( Sub
                                                      (Val $ La.Value 9)
                                                      (Val $ La.Value 8)
                                                    )), [])
  it "Parse 5+(8*9)-3" $
    shouldBe (parseExpression $ lexer "5+(8*9)-3") (Right
                                                    ( Sub
                                                      ( Add
                                                        (Val $ La.Value 5)
                                                        ( Mul
                                                          (Val $ La.Value 8)
                                                          (Val $ La.Value 9)
                                                        )
                                                      )
                                                      (Val $ La.Value 3)
                                                    ), [])
  it "Parse 5*(9-8)/3 + 2" $
    shouldBe
      (parseExpression $ lexer "5*(9-8)/3 + 2")
      ( Right ( Add
                ( Div
                  ( Mul
                    (Val $ La.Value 5)
                    ( Sub
                      (Val $ La.Value 9)
                      (Val $ La.Value 8)
                    )
                  )
                  (Val $ La.Value 3)
                )
                (Val $ La.Value 2)
              ), [])
    
