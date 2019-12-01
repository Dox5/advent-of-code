import Lib

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "calcFuelRequirement" $ do
    context "weight 12" $ do
      it "will require 2 fuel" $ do
        calcFuelRequirement 12 `shouldBe` 2

    context "weight 14" $ do
      it "will require 4 fuel" $ do
        calcFuelRequirement 14 `shouldBe` 2

    context "weight 1969" $ do
      it "will require 654 fuel" $ do
        calcFuelRequirement 1969 `shouldBe` 654

    context "weight 100756" $ do
      it "will require 33583 fuel" $ do
        calcFuelRequirement 100756 `shouldBe` 33583
  
  describe "calcCompoundFuelRequirement"  $ do
    context "weight 14" $ do
      it "will require 2 fuel" $ do
        calcCompoundFuelRequirement 12 `shouldBe` 2

    context "weight 1969" $ do
      it "will require 966 fuel" $ do
        calcCompoundFuelRequirement 1969 `shouldBe` 966

    context "weight 100756" $ do
      it "will require 50346 fuel" $ do
        calcCompoundFuelRequirement 100756 `shouldBe` 50346

