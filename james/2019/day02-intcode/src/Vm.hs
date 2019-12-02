module Vm (
  Program (Program),
  execute,
  load,
)
where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

data BinOp = Add | Mult
  deriving (Show)


data Instruction = Halt | Binary {
  op :: BinOp,
  operandA :: Int,
  operandB :: Int,
  dest     :: Int
}
  deriving (Show)

data Program = Program (IntMap Int)
  deriving (Show, Eq)

(!) :: Program -> Int -> Int
(!) (Program memory) idx = memory IntMap.! idx

writeTo :: Program -> Int -> Int -> Program
writeTo (Program memory) idx val = Program . IntMap.insert idx val $ memory

makeResult :: Program -> [Int]
makeResult (Program memory) = map (\(_, v) -> v) . IntMap.toList $ memory

decodeBinOp :: BinOp -> Int -> Program -> Instruction
decodeBinOp op ip prog =
  let
    operandA = prog ! (ip + 1)
    operandB = prog ! (ip + 2)
    dest = prog ! (ip + 3)
  in Binary op operandA operandB dest

fullDecode :: Int -> Int -> Program -> Instruction
fullDecode 99 _ _ = Halt
fullDecode 1 ip prog = decodeBinOp Add ip prog
fullDecode 2 ip prog = decodeBinOp Mult ip prog

decode :: Int -> Program -> Instruction
decode ip prog =
  let
    opcode = prog ! ip
  in fullDecode opcode ip prog

getBinaryOpImpl :: BinOp -> (Int -> Int -> Int)
getBinaryOpImpl Add  = (+)
getBinaryOpImpl Mult = (*)

-- Returns the program after 1 step of execution and if it has completed
executeInstruction :: Instruction -> Program -> (Program, Bool)
executeInstruction Halt prog = (prog, True)
executeInstruction (Binary op operandA operandB dest) prog =
  let
    impl = getBinaryOpImpl op
    valA = prog ! operandA
    valB = prog ! operandB
    computed = impl valA valB
  in ((writeTo prog dest computed), False)


executeRecurse :: Int -> Program -> Program
executeRecurse ip prog =
  let
    inst = decode ip prog
    (updated, halted) = executeInstruction inst prog
  in if halted then prog else executeRecurse (ip + 4) updated

-- Execute given program until a halt and return resultant program/result
execute :: Program -> [Int]
execute prog =
  let
    finalProg = executeRecurse 0 prog
  in  makeResult finalProg

load :: [Int] -> Program
load instructions = Program . IntMap.fromList .  zip [0 :: Int ..] $ instructions
