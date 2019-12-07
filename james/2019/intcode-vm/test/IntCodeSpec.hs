module IntCodeSpec where

import qualified IntCode as Vm

import Test.Hspec

loadAndRun :: [Int] -> Vm.Result
loadAndRun = Vm.run . Vm.load

leftResultIs :: Int -> Vm.Result -> Bool
leftResultIs expected = (expected ==) . Vm.left

spec :: Spec
spec = do

  describe "load and run simple programs" $ do
    context "simplest program" $ do
      it "should have left result 99" $ do
        loadAndRun [99] `shouldSatisfy` leftResultIs 99

    context "add numbers" $ do
      it "should have left result 2" $ do
        loadAndRun [1, 0, 0, 0, 99] `shouldSatisfy` leftResultIs 2

    context "several instructions" $ do
      it "should have left result 5*2+1 (11)" $ do
        loadAndRun [2, 9, 10, 0, 1, 0, 4, 0, 99, 5, 2] `shouldSatisfy` leftResultIs 11
