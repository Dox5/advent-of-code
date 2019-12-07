-- Instruction decoder
module IntCode.Decoder where

import IntCode.Program

-- The longest instruction (including op-codes
longestInstruction :: Int
longestInstruction = 4

data BinOp = Add | Mult
  deriving (Show)

data Instruction = Halt | Binary {
  op :: BinOp,
  operandA :: Int,
  operandB :: Int,
  dest     :: Int
}
  deriving (Show)

decodeBinOp :: BinOp -> [Int] -> Instruction
decodeBinOp op (a:b:dest:[]) = Binary op a b dest
decodeBinOp _ _ = error "Not enough operands for Binary Operation"

fullDecode :: Int -> [Int] -> Instruction
fullDecode 99 _ = Halt
fullDecode 1 operands = decodeBinOp Add operands
fullDecode 2 operands = decodeBinOp Mult operands
fullDecode x _ = error $ "wtf is " ++ show x


-- prog is a section of the program, first value is expected to the be op code
decode :: [Int] -> Instruction
decode (opcode:operands) = fullDecode opcode operands


