module VmSpec (
  testVm
)
where

import qualified Vm as Vm

import Test.Hspec

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

testExecute :: [Int] -> [Int] -> Spec
testExecute input result = context ("execute proram " ++ show input) $ do
  it ("will produce result " ++ show result) $ do
    Vm.execute (Vm.load input) `shouldBe` result

testLoad :: [Int] -> IntMap Int -> Spec
testLoad input result = context ("load proram " ++ show input) $ do
  it ("will produce program " ++ show result) $ do
    Vm.load input `shouldBe` Vm.Program result

testVm :: IO ()
testVm = hspec $ do
  describe "load" $ do
    testLoad [1, 1, 1, 1] (IntMap.fromList [(0, 1), (1, 1), (2, 1), (3, 1)])
    testLoad [1, 2, 3, 5] (IntMap.fromList [(0, 1), (1, 2), (2, 3), (3, 5)])

  describe "execute" $ do
    testExecute [1, 0, 0, 0, 99] [2, 0, 0, 0, 99]
    testExecute [2, 3, 0, 3, 99] [2, 3, 0, 6, 99]
    testExecute [2,4,4,5,99,0] [2,4,4,5,99,9801]
    testExecute [1, 1, 1, 4, 99, 5, 6, 0, 99] [30, 1, 1, 4, 2, 5, 6, 0, 99]

