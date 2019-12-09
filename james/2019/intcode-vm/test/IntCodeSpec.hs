module IntCodeSpec where

import qualified IntCode as Vm

import Test.Hspec

loadAndRun :: [Int] -> Vm.Result
loadAndRun = loadAndRunWithInput []

loadAndRunWithInput :: [Int] -> [Int] -> Vm.Result
loadAndRunWithInput input = Vm.runWithInput input . Vm.load

leftResultIs :: Int -> Vm.Result -> Bool
leftResultIs expected = (expected ==) . Vm.left

outputIs :: [Int] -> Vm.Result -> Bool
outputIs expected = (expected ==) . Vm.output

spec :: Spec
spec = do

  describe "load and run simple programs" $ do
    context "simplest program" $ do
      it "should have left result 99" $ do
        loadAndRun [99] `shouldSatisfy` leftResultIs 99

    context "add numbers" $ do
      it "should have left result 2" $ do
        loadAndRun [1, 0, 0, 0, 99] `shouldSatisfy` leftResultIs 2

      it "should support negative immediate values" $ do
        loadAndRun [1101, -5, 10, 0, 99] `shouldSatisfy` leftResultIs 5


    context "several instructions" $ do
      it "should have left result 5*2+1 (11)" $ do
        loadAndRun [2, 9, 10, 0, 1, 0, 4, 0, 99, 5, 2] `shouldSatisfy` leftResultIs 11

    context "program with immediate instructions" $ do
      it "should produce result 5001" $ do
        loadAndRun
          [102, 2500, 9, 0, 1001, 0, 1, 0, 99, 2]
        `shouldSatisfy` leftResultIs 5001

    context "program with output" $ do
      it "should populate outputs" $ do
        loadAndRun
          [1101, 42, 0, 5001, 4, 5001, 99]
        `shouldSatisfy` outputIs [42]

      it "should return outputs in order" $ do
        loadAndRun
          [1101, 55, 0, 5001, 1101, 12, 0, 5002, 4, 5001, 4, 5002, 99]
        `shouldSatisfy` outputIs [55, 12]

    context "program with input" $ do
      it "should read input" $ do
        loadAndRunWithInput [5]
          [3, 0, 99]
        `shouldSatisfy` leftResultIs 5

    context "output an immediate" $ do
      it "should output operand" $ do
        loadAndRun
          [104, 2, 99]
        `shouldSatisfy` outputIs [2]

    context "less-than comparison" $ do
      it "should perform less-than" $ do
        loadAndRun
          [1107, 10, 5, 100, 1107, 7, 13, 101, 4, 100, 4, 101, 99]
        `shouldSatisfy` outputIs [0, 1]

    context "equal-to comparison" $ do
      it "should perform equals" $ do
        loadAndRun
          [1108, 9, 10, 100, 1108,  10, 10, 101, 4, 100, 4, 101, 99]
        `shouldSatisfy` outputIs [0, 1]

    context "multiply two numbers" $ do
      it "should multiple two input numbers and output" $ do
        loadAndRunWithInput [9, 100]
          [3, 100, 3, 101, 2, 100, 101, 100, 4, 100, 99]
        `shouldSatisfy` outputIs [900]

    context "jump if true" $ do
      it "should jump past initial halt" $ do
        loadAndRun
          [1105, 1, 4, 99, 104, 17, 99]
        `shouldSatisfy` outputIs [17]

    context "jump if false" $ do
      it "should not jump past initial halt" $ do
        loadAndRun
          [1106, 1, 4, 99, 104, 17, 99]
        `shouldSatisfy` outputIs []

    context "change relative base and relative operand" $ do
      it "should correctly ouput 97" $ do
        loadAndRun
          [109, 4, 204, 2, 99, 98, 97]
        `shouldSatisfy` outputIs [97]
