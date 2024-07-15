
module CalcLexerASTSpec where
import Test.Hspec
import Data.Maybe

import qualified Calc.Lexer.AST as AST
import qualified Calc.Lexer.Internal as Li
import qualified Calc.Equation.Internal as Eq
import qualified Calc.Lexer as Lexer

spec :: Spec
spec = do
  describe "Test the negate number function" $ do
    it "Real Positive value" $
        (AST.negateNumber $ Li.RealValue 5.89) `shouldBe` (AST.Val (Li.RealValue (-5.89)))
    it "Real Positive value" $
        (AST.negateNumber $ Li.RealValue (-5.89)) `shouldBe` (AST.Val (Li.RealValue (5.89)))
    it "Natural number" $
      (AST.negateNumber $ Li.RealValue 57) `shouldBe` (AST.Val $ Li.RealValue (-57))
  describe "Test the parse value function" $ do
    it "With tokens conversion" $ do
      (AST.parseValue $ Lexer.lex' "-57") `shouldBe` (Right $ AST.Val $ Li.RealValue (-57), [])
      (AST.parseValue $ Lexer.lex' "57") `shouldBe` (Right $ AST.Val $ Li.RealValue (57), [])

  describe "Test the lex' function" $ do
    it "Simple operation with parens" $
      shouldBe
        (Lexer.lex' "(5+9)")
        [
          Li.TokLeftParen
        , Li.TokNumber $ Li.RealValue 5
        , Li.TokOperator Eq.Sumatory
        , Li.TokNumber $ Li.RealValue 9
        , Li.TokRightParen
        ]
    it "Simple operation without parens" $
      shouldBe
        (Lexer.lex' "5 + 9")
        [
          Li.TokNumber $ Li.RealValue 5
        , Li.TokOperator Eq.Sumatory
        , Li.TokNumber $ Li.RealValue 9
        ]
    it "Simple operation without parens and multiple operations" $
      shouldBe
        (Lexer.lex' "5 + -9")
        [
          Li.TokNumber $ Li.RealValue 5
        , Li.TokOperator Eq.Sumatory
        , Li.TokOperator Eq.Difference
        , Li.TokNumber $ Li.RealValue 9
        ]

  describe "Test parseValue function" $ do
    it "Single token" $
      shouldBe
        (AST.parseValue [Li.TokNumber $ Li.RealValue 5])
        (Right $ AST.Val $ Li.RealValue 5, [])

    it "Single token with big number" $
      let
        rv = Li.RealValue 58
      in
        shouldBe
          (AST.parseValue [Li.TokNumber $ rv])
          (Right $ AST.Val $ rv, [])
    it "Double token with negative number" $
      let
        rv = Li.RealValue 58
        nrv = Li.RealValue (-58)
      in
        shouldBe
          (AST.parseValue [Li.TokOperator Eq.Difference, Li.TokNumber $ rv])
          (Right $ AST.Val nrv, [])

    it "Double token with negative number and remainder" $
      let
        rv = Li.RealValue 58
        nrv = Li.RealValue (-58)
      in
        shouldBe
          (AST.parseValue
            [
              Li.TokOperator Eq.Difference
            , Li.TokNumber $ rv
            , Li.TokLeftParen
            , Li.TokNumber $ Li.RealValue 5
            , Li.TokRightParen
            ])
          (Right $ AST.Val $ nrv,
            [ Li.TokLeftParen
            , Li.TokNumber $ Li.RealValue 5
            , Li.TokRightParen])
    it "Invalid token list"  $
      shouldBe
        (AST.parseValue
          [
            Li.TokLeftParen
          , Li.TokNumber $ Li.RealValue 58
          , Li.TokRightParen
          ])
        (Left $ AST.MissingVal [
            Li.TokLeftParen
          , Li.TokNumber $ Li.RealValue 58
          , Li.TokRightParen
                               ], [
            Li.TokLeftParen
          , Li.TokNumber $ Li.RealValue 58
          , Li.TokRightParen
          ])


  describe "Parse Expression" $ do
    it "Use the lex' function to convert a string into a expression" $
      let
        f5 = AST.Val $ Li.RealValue 5
        f8 = AST.Val $ Li.RealValue 8
        f9 = AST.Val $ Li.RealValue 9
        f3 = AST.Val $ Li.RealValue 3
        f2 = AST.Val $ Li.RealValue 2
      in
        do
          shouldBe
            (AST.parseExpression $ Lexer.lex' "5+8")
            (Right $ AST.Add f5 f8, [])
          shouldBe
            (AST.parseExpression $ Lexer.lex' "5*8+9")
            (Right $ AST.Add (AST.Mul f5 f8) f9, [])
          shouldBe
            (AST.parseExpression $ Lexer.lex' "5+9-8")
            (Right $ AST.Add f5 (AST.Sub f9 f8), [])
          shouldBe
            (AST.parseExpression $ Lexer.lex' "5*(9-8)")
            (Right $ AST.Mul f5 (AST.Sub f9 f8), [])
          shouldBe
            (AST.parseExpression $ Lexer.lex' "5+(8*9)-3")
            (Right $ AST.Add f5 (AST.Sub (AST.Mul f8 f9) f3), [])
          shouldBe
            (AST.parseExpression $ Lexer.lex' "5*(9-8)/3 + 2")
            (Right $ AST.Add
              (AST.Mul f5 (AST.Div (AST.Sub f9 f8) f3))
              f2
            , []
            )
    it "Use the lex' function to convert a string into a expression with complex priorities" $
      let
        f2 = AST.Val $ Li.RealValue 2
        f3 = AST.Val $ Li.RealValue 3
        f4 = AST.Val $ Li.RealValue 4
        f5 = AST.Val $ Li.RealValue 5
        f8 = AST.Val $ Li.RealValue 8
        f9 = AST.Val $ Li.RealValue 9
        f10 = AST.Val $ Li.RealValue 10
      in
        shouldBe
          (AST.parseExpression $ Lexer.lex' "9 - (4*10) + 3 -5")
          (
            Right (AST.Sub
                      (AST.Add
                        (AST.Sub f9
                          (AST.Mul f4 f10))
                        f3
                      )
                    f5
                  )
          , []
          )
