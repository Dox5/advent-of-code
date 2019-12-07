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

initialVmState :: Program -> VmState
initialVmState p = VmState {
    ip = 0,
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

incrementIp :: Int -> State VmState Int
incrementIp amount = do
  vm <- State.get 
  let ip' = (ip vm) + amount
  State.put vm{ip = ip'}
  return ip'

setHalt :: State VmState ()
setHalt = do
  vm <- State.get
  State.put vm{halt = True}
  return ()

execute :: Decoder.Instruction -> State VmState ()

execute Decoder.Halt = setHalt

execute (Decoder.Binary op opA opB (Decoder.Positional dest)) = do
  valA <- evalInputOperand opA
  valB <- evalInputOperand opB
  let
    impl = getBinaryOpImpl op
    computed = impl valA valB

  writeTo dest computed
  incrementIp 4
  return ()

execute (Decoder.OneOp Decoder.Output operand) = do
  val <- evalInputOperand operand
  doOutput val
  incrementIp 2
  return ()

execute (Decoder.OneOp Decoder.Input (Decoder.Positional address)) = do
  val <- doInput
  writeTo address val
  incrementIp 2
  return ()

execute instruction = error $ "Illegal instruction" ++ show instruction

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
runWithInput input p = State.evalState step (setInput input . initialVmState $ p)
