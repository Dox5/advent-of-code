import Lib

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "explode" $ do
    context "single digit" $ do
      it "will produce [1]" $ do
        explode 1 `shouldBe` [1]

    context "multiple digits" $ do
      it "will prodice [1, 2, 3, 4, 5]" $ do
        explode 12345 `shouldBe` [1, 2, 3, 4, 5]

  describe "password requirements" $ do
    context "fulfils requirements" $ do
      it "will pass" $ do
        11111 `shouldSatisfy` potentialPassword

    context "decreasing digits" $ do
      it "will not pass" $ do
        223450 `shouldNotSatisfy` potentialPassword

    context "no double" $ do
      it "will not pass" $ do
        123789 `shouldNotSatisfy` potentialPassword

  describe "password requirements w/no large groups" $ do
    context "fulfils requirements" $ do
      it "will pass" $ do
        112233 `shouldSatisfy` potentialPassword2

    context "decreasing digits" $ do
      it "will not pass" $ do
        223450 `shouldNotSatisfy` potentialPassword2

    context "larger group" $ do
      it "will not pass" $ do
        123444 `shouldNotSatisfy` potentialPassword2

    context "no double" $ do
      it "will not pass" $ do
        123789 `shouldNotSatisfy` potentialPassword2

    context "large group and double" $ do
      it "will not pass" $ do
        111122 `shouldSatisfy` potentialPassword2
