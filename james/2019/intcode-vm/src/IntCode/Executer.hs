module IntCode.Executer where

import qualified IntCode.Decoder as Decoder
import IntCode.Program

import Control.Monad.State (State)
import qualified Control.Monad.State as State

-- Used to construct an output list (in order)
type OutputS = [Int] -> [Int]

-- Holds the current execution state
data VmState = VmState {
  prog :: Program,
  halt :: Bool,
  ip :: Int,
  basePtr :: Int,
  outputChain :: OutputS,
  input :: [Int]
}


data Result = Result {
  -- Left most int result
  left :: Int,

  -- Result of any output instructions, in chronological order
  output :: [Int]
}
  deriving (Show)

-- Memory write access
writeTo :: Int -> Int -> State VmState ()
writeTo address value = do
  vm <- State.get
  let
    p = prog vm
    p' = setAddress p address value
  State.put vm{prog = p'}

readFrom :: Int -> State VmState Int
readFrom address = do
  vm <- State.get
  let
    p = prog vm
    v = p ! address
  return . seq v $ v -- Ensure that memory accesses are validated at this point

doOutput :: Int -> State VmState ()
doOutput v = do
  vm <- State.get
  let
    chain = outputChain vm
    chainLink = (\l -> v:l)
    chain' = chain . chainLink
  State.put vm{outputChain = chain'}

doInput :: State VmState Int
doInput = do
  vm <- State.get
  case input vm of
    (v:rest) -> State.put vm {input = rest} >> return v
    []       -> error "Requsted input but had nothing to give" 

getBasePtr :: State VmState Int
getBasePtr = do
  vm <- State.get
  return $ basePtr vm

setBasePtr :: Int -> State VmState ()
setBasePtr base = do
  vm <- State.get
  State.put vm{basePtr = base}
  return ()

makeResult :: State VmState Result
makeResult = do
  vm <- State.get
  return Result {
    left = (prog vm) ! 0,
    output = (outputChain vm) $! []
  }

-- Get the value for an input operand
evalInputOperand :: Decoder.Operand -> State VmState Int
evalInputOperand (Decoder.Immediate v) = return v
evalInputOperand (Decoder.Positional address) = do readFrom address
evalInputOperand (Decoder.Relative offset) = do
  base <- getBasePtr
  readFrom (base + offset)

evalDestOperand :: Decoder.Operand -> State VmState Int
evalDestOperand (Decoder.Immediate _) = error "Destination operand cannot be immediate"
evalDestOperand (Decoder.Positional address) = return address
evalDestOperand (Decoder.Relative offset) = do
  base <- getBasePtr
  return (base + offset)

initialVmState :: Program -> VmState
initialVmState p = VmState {
    ip = 0,
    basePtr = 0,
    prog = p,
    halt = False,
    outputChain = id,
    input = []
  }

setInput :: [Int] -> VmState -> VmState
setInput is vm = vm {input = is}

getBinaryOpImpl :: Decoder.BinOp -> (Int -> Int -> Int)
getBinaryOpImpl Decoder.Add  = (+)
getBinaryOpImpl Decoder.Mult = (*)
getBinaryOpImpl Decoder.LessThan = (\l r -> if l < r then 1 else 0)
getBinaryOpImpl Decoder.Equal = (\l r -> if l == r then 1 else 0)

getJumpTest :: Decoder.JmpWhen -> (Int -> Bool)
getJumpTest Decoder.NonZero = (0/=)
getJumpTest Decoder.Zero = (0==)


incrementIp :: Int -> State VmState ()
incrementIp amount = do
  vm <- State.get 
  let ip' = (ip vm) + amount
  State.put vm{ip = ip'}
  return ()

setIp :: Int -> State VmState ()
setIp to = do
  vm <- State.get 
  State.put vm{ip = to}
  return ()

setHalt :: State VmState ()
setHalt = do
  vm <- State.get
  State.put vm{halt = True}
  return ()

execute :: Decoder.Instruction -> State VmState ()

execute Decoder.Halt = setHalt

execute (Decoder.Binary op opA opB opD) = do
  valA <- evalInputOperand opA
  valB <- evalInputOperand opB
  valD <- evalDestOperand opD
  let
    impl = getBinaryOpImpl op
    computed = impl valA valB

  writeTo valD computed
  incrementIp 4
  return ()

execute (Decoder.OneOp Decoder.Output operand) = do
  val <- evalInputOperand operand
  doOutput val
  incrementIp 2
  return ()

execute (Decoder.OneOp Decoder.Input opD) = do
  val <- doInput
  valD <- evalDestOperand opD
  writeTo valD val
  incrementIp 2
  return ()

execute (Decoder.OneOp Decoder.ChangeBasePtr opA) = do
  val <- evalInputOperand opA
  base <- getBasePtr
  setBasePtr (base + val)
  incrementIp 2
  return ()

execute (Decoder.Jmp when opCond opTo) = do
  val <- evalInputOperand opCond
  let check = getJumpTest when

  if check val then
    do
      target <- evalInputOperand opTo
      setIp target
  else
    incrementIp 3

  return ()
  
fetchDecode :: State VmState Decoder.Instruction
fetchDecode =
  do
    vm <- State.get
    let p = prog vm
    -- Get the values for (potentially) the longest instruction, lazy evaluation
    -- means that the list will always be the right length (even if the extra
    -- elements cause an error if ever evaluated). This allows a HALT at the end
    -- of a program to be handled in the same way as a full size instruction
    return $ Decoder.decode $ [p ! ((ip vm) + offset) | offset <- [0..Decoder.longestInstruction-1]]

step :: State VmState Result
step = do
  inst <- fetchDecode
  execute inst
  vm <- State.get 
  if halt vm then
    makeResult
  else
    step  

run :: Program -> Result
run p = State.evalState step (initialVmState p)

runWithInput :: [Int] -> Program -> Result
runWithInput i p = State.evalState step (setInput i . initialVmState $ p)
