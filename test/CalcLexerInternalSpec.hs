module CalcLexerInternalSpec where
import Test.Hspec
import Data.Maybe

import qualified Calc.Lexer.Internal as Li

spec :: Spec
spec = do
    describe "Test the output string" $ do
        it "operations string" $ do
            show Li.Summ `shouldBe` "addop +"
            show Li.Diff `shouldBe` "addop -"
            show Li.Mult `shouldBe` "mulop *"
            show Li.Divi `shouldBe` "mulop /"
        it "Conversion between characters and operations" $ do
          Li.charToAddop '+' `shouldBe` (Right Li.Summ)
          Li.charToAddop '-' `shouldBe` (Right Li.Diff)
          Li.charToMulop '*' `shouldBe` (Right Li.Mult)
          Li.charToMulop '/' `shouldBe` (Right Li.Divi)
          Li.charToAddop 'a' `shouldBe` (Left Li.InvalidAddopSign)
          Li.charToMulop 'a' `shouldBe` (Left Li.InvalidMulopSign)

        it "Conversion between strings and values" $ do
          Li.stringToValue "58" `shouldBe` (Right (Li.RealValue 58, ""))
          Li.stringToValue "58+56" `shouldBe` (Right (Li.RealValue 58, "+56"))
          (Li.stringToValue $ Li.cleanString "58 + 56") `shouldBe` (Right (Li.RealValue 58, "+56"))
          (Li.stringToValue $ Li.cleanString "a + ;") `shouldBe` (Left Li.InvalidNumber)

        it "Conversion between strings to Factor" $ do
          Li.evalFactor "0.58" `shouldBe` (Right (Li.Factor $ Li.RealValue 0.58, ""))
          Li.evalFactor "58.85+56" `shouldBe` (Right (Li.Factor $ Li.RealValue 58.85, "+56"))
          (Li.evalFactor $ Li.cleanString ".58 + 56") `shouldBe` (Right (Li.Factor $ Li.RealValue 0.58, "+56"))
    describe "Test terms composition" $ do
      it "Only use evalTerm'" $ do
        Li.evalTerm' Nothing "7+95" `shouldBe` Right ([], "7+95")
        Li.evalTerm' Nothing "*95" `shouldBe` Right ([(Li.Mult, Li.Factor $ Li.RealValue 95)], "")
        Li.evalTerm' Nothing "*95/5+953" `shouldBe` Right ([
                                                              (Li.Mult, Li.Factor $ Li.RealValue 95),
                                                              (Li.Divi, Li.Factor $ Li.RealValue 5)
                                                            ], "+953")
      it "Only use evalTerm" $ do
        Li.evalTerm "7*5+3" `shouldBe` Right
          (
            Li.Term (Li.Factor $ Li.RealValue 7, [(Li.Mult, Li.Factor $ Li.RealValue 5)]) , "+3"
          )
    describe "Test eval expressions" $ do
      it "Using evalExpression' with one element" $
        let
          sum' = Li.Summ
          f5 = Li.Factor $ Li.RealValue 5
          term = Li.Term (f5, [])
        in
          Li.evalExpression' Nothing "+5" `shouldBe` Right ([(sum', term)], "")
      it "Using evalExpression' with two elements" $
        let
          sum' = Li.Summ
          f5 = Li.Factor $ Li.RealValue 5
          diff = Li.Diff
          f9 = Li.Factor $ Li.RealValue 9
          term1 = Li.Term (f5, [])
          term2 = Li.Term (f9, [])
        in
          Li.evalExpression' Nothing "+5-9" `shouldBe` Right ([
                                                               (sum', term1),
                                                               (diff, term2)
                                                            ], "")
      it "Using eval expression with a complex structure" $
        let
          sum' = Li.Summ
          diff = Li.Diff
          mul = Li.Mult
          f5 = Li.Factor $ Li.RealValue 5
          f9 = Li.Factor $ Li.RealValue 9
          f58 = Li.Factor $ Li.RealValue 58
          t1 = Li.Term (f5, [])
          t2 = Li.Term (f9, [(mul, f58)])
        in
          Li.evalExpression' Nothing "+5-9*58" `shouldBe` Right ([
                                                                    (sum', t1),
                                                                    (diff, t2)
                                                            ], "")
      it "Eval full expression" $
        let
          f4 = Li.Factor $ Li.RealValue 4
          f9 = Li.Factor $ Li.RealValue 9
          term1 = Li.Term (f4, [])
          term2 = Li.Term (f9, [])
          expr = (term1, [(Li.Diff, term2)])
        in
          show (Li.eval "4-9") `shouldBe` (show $ Li.Expression expr)
      it "Eval more complex expression" $ 
        let
          f3 = Li.Factor $ Li.RealValue 3
          f4 = Li.Factor $ Li.RealValue 4
          f6 = Li.Factor $ Li.RealValue 6
          term1 = Li.Term (f4, [(Li.Mult, f6)])
          term2 = Li.Term (f3, [])
          expr = (term1, [(Li.Summ, term2)])
        in
          show (Li.eval "4*6+3") `shouldBe` (show $ Li.Expression expr)
