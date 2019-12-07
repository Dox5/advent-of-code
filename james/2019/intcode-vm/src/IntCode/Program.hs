-- Program/memory storage
module IntCode.Program where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import qualified Data.IntMap.Strict as IntMap

data Program = Program (IntMap Int)
  deriving (Show, Eq)

(!) :: Program -> Int -> Int
(!) (Program memory) idx = memory IntMap.! idx

load :: [Int] -> Program
load instructions = Program . IntMap.fromList .  zip [0 :: Int ..] $ instructions

setAddress :: Program -> Int -> Int -> Program
setAddress (Program m) i v =
  let
    m' = IntMap.insert i v m
  in Program m'

makeResult :: Program -> [Int]
makeResult (Program memory) = map (\(_, v) -> v) . IntMap.toList $ memory
