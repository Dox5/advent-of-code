module IntCode.DecoderSpec where

import IntCode.Decoder

import Test.Hspec

import qualified Control.DeepSeq as DeepSeq
import qualified Control.Exception as Exception


makeTest :: String -> Instruction -> [Int] -> Spec
makeTest ctx expected input = context ((ctx ++) . showString ": ". show $ input) $ do
  it "should decode correctly" $ do
    (decode input) `shouldBe` expected

makeErrorTest :: String -> [Int] -> Spec
makeErrorTest ctx input = context ((ctx ++) . showString ": ". show $ input) $ do
  it "should produce an error" $ do
    (Exception.evaluate $ DeepSeq.force (decode input)) `shouldThrow` anyErrorCall

spec :: Spec
spec = do
  describe "no operand instructions" $ do
    makeTest "halt instruction" Halt [99]

  describe "binary operation decoding" $ do
    makeTest "add instruction"
             (Binary Add (Positional 1) (Positional 2) (Positional 3))
             [1, 1, 2, 3]

    makeTest "multiply instruction"
             (Binary Mult (Positional 5) (Positional 6) (Positional 7))
             [2, 5, 6, 7]

    makeTest "less than instruction"
             (Binary LessThan (Positional 8) (Positional 9) (Positional 10))
             [7, 8, 9, 10]

    makeTest "equals instruction"
             (Binary Equal (Positional 11) (Positional 12) (Positional 13))
             [8, 11, 12, 13]

  describe "oneop operation decoding" $ do
    makeTest "input instruction"
             (OneOp Input (Positional 50))
             [3, 50]

    makeTest "output instruction"
             (OneOp Output (Positional 75))
             [4, 75]

    makeTest "change relative base"
             (OneOp ChangeBasePtr (Positional 75))
             [9, 75]

  describe "Jump instructions" $ do
    makeTest "jump if true, positional"
      (Jmp NonZero (Positional 25) (Positional 100))
      [5, 25, 100]

    makeTest "jump if false, positional"
      (Jmp Zero (Positional 26) (Positional 101))
      [6, 26, 101]

    makeTest "jump if true, immediate"
      (Jmp NonZero (Positional 7) (Immediate 3))
      [1005, 7, 3]

  describe "invalid instructions" $ do
    makeErrorTest "unknown instruction" [67, 5, 1, 2]
    makeErrorTest "too few operands" [3]
    makeErrorTest "too few operands" [1, 0, 0]


  describe "operand mode decoding" $ do
    makeTest "all positional"
             (Binary Add (Positional 5) (Positional 5) (Positional 5))
             [1, 5, 5, 5]

    makeTest "immediate and implicit positional"
             (Binary Mult (Positional 2) (Immediate 3) (Positional 4))
             [1002, 2, 3, 4]

    makeTest "only immediate"
             (Binary Mult (Immediate 7) (Immediate 8) (Positional 9))
             [1102, 7, 8, 9]

    makeTest "immediate and explict positional"
             (Binary Mult (Positional 17) (Immediate 18) (Positional 14))
             [1002, 17, 18, 14]

    makeTest "immediate output"
             (OneOp Output (Immediate 181))
             [104, 181]

    makeTest "output with relative"
             (OneOp Output (Relative 32))
             [204, 32]

    makeTest "halt with mode still halt" Halt [199]

    makeErrorTest "binary destination cannot be immediate"
                  [10002, 1, 1, 0]

    makeErrorTest "one op destination cannot be immeidate"
                  [103, 1]
