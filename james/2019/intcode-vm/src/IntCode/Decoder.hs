-- Instruction decoder
module IntCode.Decoder where

import IntCode.Program

import qualified Control.DeepSeq as DeepSeq

-- The longest instruction (including op-codes
longestInstruction :: Int
longestInstruction = 4

data Operand = Positional Int | Immediate Int
  deriving (Show, Eq)

data BinOp = Add | Mult | LessThan | Equal
  deriving (Show, Eq)

data SingleOp = Input | Output
  deriving (Show, Eq)

data Instruction = Halt |
  Binary {
    binOp :: BinOp,
    operandA :: Operand,
    operandB :: Operand,
    dest     :: Operand
  } |
  
  -- Terrible name, not urinary as it has no dest param!
  OneOp {
    singleOp :: SingleOp,
    operandA :: Operand
  }
  deriving (Show, Eq)

instance DeepSeq.NFData BinOp where
  rnf _ = ()

instance DeepSeq.NFData SingleOp where
  rnf _ = ()

instance DeepSeq.NFData Operand where
  rnf (Positional x) = (DeepSeq.rnf x)
  rnf (Immediate  x) = (DeepSeq.rnf x)

instance DeepSeq.NFData Instruction where
  rnf (Halt) = ()
  rnf (Binary op a b d) = DeepSeq.rnf (op, a, b, d)
  rnf (OneOp op a) = DeepSeq.rnf (op, a)

-- Raw modes decoded from the instruction
newtype OpMode = OpMode Int
  deriving (Show)

-- Operation value (stripped of OpModes or other such things)
newtype CodePoint = CodePoint Int
  deriving (Show)

decodeOperand :: OpMode -> Int -> Operand
decodeOperand (OpMode 0) v = Positional v
decodeOperand (OpMode 1) v = Immediate v
decodeOperand x _ = error $ "Decode failed unknown OpMode: " ++ show x

decodeOperandPositionalOnly :: OpMode -> Int -> Operand
decodeOperandPositionalOnly m v =
  let
    decoded = decodeOperand m v
  in case decoded of
       (Positional _) -> decoded 
       op -> error $ "Decode failed only positional address mode allowed here, value was " ++ show op

splitOp :: Int -> (CodePoint, [OpMode])
splitOp op =
  let
    operation = op `rem` 100
    modes = [OpMode $ (op `div` x) `rem` 10 | x <- [100, 1000, 10000]]
  in (CodePoint operation, modes)

decodeBinOp :: BinOp -> [(OpMode, Int)] -> Instruction
decodeBinOp op (a:b:dest:[]) =
  let
    opA  = (uncurry decodeOperand) a
    opB  = (uncurry decodeOperand) b
    opDest = (uncurry decodeOperandPositionalOnly) dest
  in Binary op opA opB opDest
decodeBinOp _ _ = error "Not enough operands for Binary Operation"

decodeOneOp :: SingleOp -> [(OpMode, Int)] -> Instruction
decodeOneOp op (operand:_) = (OneOp op (dec operand))
  where
    dec = case op of
            Input  -> uncurry decodeOperandPositionalOnly
            Output -> uncurry decodeOperand
decodeOneOp op [] = error "Decode failed not enough operands"

fullDecode :: (CodePoint, [OpMode]) -> [Int] -> Instruction
fullDecode (code, modes) operandValues = 
  let
     operands = zip modes operandValues
  in case code of
    (CodePoint 99) -> Halt
    (CodePoint  1) -> decodeBinOp Add operands
    (CodePoint  2) -> decodeBinOp Mult operands
    (CodePoint  3) -> decodeOneOp Input operands
    (CodePoint  4) -> decodeOneOp Output operands
    (CodePoint  7) -> decodeBinOp LessThan operands
    (CodePoint  8) -> decodeBinOp Equal operands
    (CodePoint  x) -> error $ "Decode failed unknown CodePoint: " ++ show x



-- prog is a section of the program, first value is expected to the be op code
decode :: [Int] -> Instruction
decode (opcode:operands) = fullDecode (splitOp opcode) operands


