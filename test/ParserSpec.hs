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
import DummyCalc.Parser.Internal
import DummyCalc.Parser.Data.Internal
import qualified DummyCalc.Language as La

spec :: Spec
spec = do
  describe "Parse some values" $ do
    it "Parse value 5" $
      shouldBe ( parseVal [TokNumber $ La.NumValue 5]) (Right $ Val $ La.NumValue 5, [])
    it "Parse value -5" $
      shouldBe ( parseVal [TokOperator La.Difference, TokNumber $ La.NumValue 5]) (Right $ Val $ La.NumValue (-5), [])
    it "Parse value 58 + 56" $
      shouldBe ( parseVal
                 [ TokNumber $ La.NumValue 58
                 , TokOperator La.Summatory
                 , TokNumber $ La.NumValue 56
                 ]
               ) (Right $ Val $ La.NumValue 58, [TokOperator La.Summatory , TokNumber $ La.NumValue 56])
  describe "Parse some expressions" $ do
    it "Parse 5+8" $
      shouldBe (parseExpression $ lexer "5+8") (Right $ Add (Val $ La.NumValue 5) (Val $ La.NumValue 8), [])
    it "Parse 5+8-3" $
      shouldBe (parseExpression $ lexer "5+8-3") (Right
                                                  (Sub
                                                   (Add
                                                    (Val $ La.NumValue 5) (Val $ La.NumValue 8)
                                                   )
                                                  (Val $ La.NumValue 3))
                                                  , [])
  it "Parse 5*(9-8)" $
    shouldBe (parseExpression $ lexer "5*(9-8)") (Right
                                                  (Mul (Val $ La.NumValue 5)
                                                    ( Sub
                                                      (Val $ La.NumValue 9)
                                                      (Val $ La.NumValue 8)
                                                    )), [])
  it "Parse 5+(8*9)-3" $
    shouldBe (parseExpression $ lexer "5+(8*9)-3") (Right
                                                    ( Sub
                                                      ( Add
                                                        (Val $ La.NumValue 5)
                                                        ( Mul
                                                          (Val $ La.NumValue 8)
                                                          (Val $ La.NumValue 9)
                                                        )
                                                      )
                                                      (Val $ La.NumValue 3)
                                                    ), [])
  it "Parse 5*(9-8)/3 + 2" $
    shouldBe
      (parseExpression $ lexer "5*(9-8)/3 + 2")
      ( Right ( Add
                ( Div
                  ( Mul
                    (Val $ La.NumValue 5)
                    ( Sub
                      (Val $ La.NumValue 9)
                      (Val $ La.NumValue 8)
                    )
                  )
                  (Val $ La.NumValue 3)
                )
                (Val $ La.NumValue 2)
              ), [])

  it "Parse -5 + 2 * 9 * 9 * 9" $
    shouldBe
      (parseExpression $ lexer "-5 + 2 * 9 * 9 * 9")
      (Right ( Add
               (Val $ La.NumValue (-5))
               ( Mul
                 ( Mul
                   ( Mul
                     (Val $ La.NumValue 2)
                     (Val $ La.NumValue 9)
                   )
                   (Val $ La.NumValue 9)
                 )
                 (Val $ La.NumValue 9)
               )
             ), [])

  it "Parse x*(9-8)/3 + 2" $
    shouldBe
      (parseExpression $ lexer "x*(9-8)/3 + 2")
      ( Right ( Add
                ( Div
                  ( Mul
                    (Var $ La.Variable "x")
                    ( Sub
                      (Val $ La.NumValue 9)
                      (Val $ La.NumValue 8)
                    )
                  )
                  (Val $ La.NumValue 3)
                )
                (Val $ La.NumValue 2)
              ), [])

  it "Parse x=>5; 5 + 3;;" $
    shouldBe
      (parseProgram $ lexer "x=>5;5+3;;")
      (Right ( Program
               [ Ass
                 (La.Variable "x")
                 (Val $ La.NumValue 5)
               , Statement
                 ( Add
                   (Val $ La.NumValue 5)
                   (Val $ La.NumValue 3)
                 )
               ]
             ), [])
  it "Parse EOS ';'" $
    shouldBe (parseEOS $ lexer ";") (Just EOS, [])
  it "Parse Statement 'x=>5;'" $
    shouldBe (parseStatement $ lexer "x=>5;") (Right (Ass (La.Variable "x") (Val $ La.NumValue 5)), [])
