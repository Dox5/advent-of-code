-- Instruction decoder
module IntCode.Decoder where

import qualified Control.DeepSeq as DeepSeq

-- The longest instruction (including op-codes
longestInstruction :: Int
longestInstruction = 4

data Operand = Positional Int | Immediate Int | Relative Int
  deriving (Show, Eq)

data BinOp = Add | Mult | LessThan | Equal
  deriving (Show, Eq)

data SingleOp = Input | Output | ChangeBasePtr
  deriving (Show, Eq)

data JmpWhen = Zero | NonZero
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
  } |

  Jmp {
    when :: JmpWhen,
    cond :: Operand,
    to   :: Operand
  }
  deriving (Show, Eq)

instance DeepSeq.NFData BinOp where
  rnf _ = ()

instance DeepSeq.NFData SingleOp where
  rnf _ = ()

instance DeepSeq.NFData Operand where
  rnf (Positional x) = (DeepSeq.rnf x)
  rnf (Immediate  x) = (DeepSeq.rnf x)
  rnf (Relative x)   = (DeepSeq.rnf x)

instance DeepSeq.NFData JmpWhen where
  rnf Zero = ()
  rnf NonZero = ()

instance DeepSeq.NFData Instruction where
  rnf (Halt) = ()
  rnf (Binary op a b d) = DeepSeq.rnf (op, a, b, d)
  rnf (OneOp op a) = DeepSeq.rnf (op, a)
  rnf (Jmp op w c) = DeepSeq.rnf (op, w, c)

-- Raw modes decoded from the instruction
newtype OpMode = OpMode Int
  deriving (Show)

-- Operation value (stripped of OpModes or other such things)
newtype CodePoint = CodePoint Int
  deriving (Show)

decodeOperand :: OpMode -> Int -> Operand
decodeOperand (OpMode 0) v = Positional v
decodeOperand (OpMode 1) v = Immediate v
decodeOperand (OpMode 2) v = Relative v
decodeOperand x _ = error $ "Decode failed unknown OpMode: " ++ show x

decodeOperandNotImmediate :: OpMode -> Int -> Operand
decodeOperandNotImmediate m v =
  let
    decoded = decodeOperand m v
  in case decoded of
       (Immediate _) -> error $ "Immediate operand mode not allowed here"
       _ -> decoded

splitOp :: Int -> (CodePoint, [OpMode])
splitOp op =
  let
    operation = op `rem` 100
    modes = [OpMode $ (op `div` x) `rem` 10 | x <- [100, 1000, 10000]]
  in (CodePoint operation, modes)

decodeBinOp :: BinOp -> [(OpMode, Int)] -> Instruction
decodeBinOp op (a:b:d:_) =
  let
    opA  = (uncurry decodeOperand) a
    opB  = (uncurry decodeOperand) b
    opDest = (uncurry decodeOperandNotImmediate) d
  in Binary op opA opB opDest
decodeBinOp _ _ = error "Decode failed, not enough operands for binary op"

decodeOneOp :: SingleOp -> [(OpMode, Int)] -> Instruction
decodeOneOp op (operand:_) = (OneOp op (dec operand))
  where
    dec = case op of
            Input  -> uncurry decodeOperandNotImmediate
            _ -> uncurry decodeOperand
decodeOneOp _ [] = error "Decode failed, not enough operands for oneop"

decodeJump :: JmpWhen -> [(OpMode, Int)] -> Instruction
decodeJump w (c:t:_) =
  let
    c' = (uncurry decodeOperand) c
    t' = (uncurry decodeOperand) t
  in Jmp w c' t'
decodeJump _ _ = error "Decode failed, not enough operands for jump"

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
    (CodePoint  5) -> decodeJump NonZero operands
    (CodePoint  6) -> decodeJump Zero operands
    (CodePoint  7) -> decodeBinOp LessThan operands
    (CodePoint  8) -> decodeBinOp Equal operands
    (CodePoint  9) -> decodeOneOp ChangeBasePtr operands
    (CodePoint  x) -> error $ "Decode failed unknown CodePoint: " ++ show x



-- prog is a section of the program, first value is expected to the be op code
decode :: [Int] -> Instruction
decode (opcode:operands) = fullDecode (splitOp opcode) operands
decode _ = error "Must provide slice of longestInstruction instructions to decode"

