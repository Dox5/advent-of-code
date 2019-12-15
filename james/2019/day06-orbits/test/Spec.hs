import Lib

import Test.Hspec

example1 :: String
example1 = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\n"

main :: IO ()
main = hspec $ do
  describe "example" $ do
    context "1" $ do
      it "should have a checksum of 42" $ do
        (checksum . load) example1 `shouldBe` 42
