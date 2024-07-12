module CalcSpec where

import Test.Hspec
import Calc

spec :: Spec
spec = do
  describe "Test entry point"  $ do
    it "Emtpy" $ shouldBe (process "") (0)
    it "Single value" $ shouldBe (process "-9") (-9)
    it "Two values" $ shouldBe (process "-9 + 5") (-4)
    it "Three values" $ shouldBe (process "-9 + 5 + 10") 6
    it "Three operations with priorities" $ shouldBe (process "-9 + 5 * 10") 41
    it "Three operations with multiples priorities" $ shouldBe (process "6 * -9 + 5 * 10") (-4)
    
  
