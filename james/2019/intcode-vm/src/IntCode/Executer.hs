module IntCode.Executer where

import qualified IntCode.Decoder as Decoder
import IntCode.Program

import Control.Monad.State (State)
import qualified Control.Monad.State as State


-- Used to construct an output list (in order)
type OutputS = [Int] -> [Int]

-- Holds the current execution state
data VMState = VMState {
  prog :: Program,
  ip :: Int,
  basePtr :: Int,
  input :: Maybe Int
}

data Result = Result {
  -- Left most int result
  left :: Int,

  -- Result of any output instructions, in chronological order
  output :: [Int]
}
  deriving (Show)

data Suspended = Output Int | InputRequested | Halted
  deriving (Show)

-- Memory write access
writeTo :: Int -> Int -> State VMState ()
writeTo address value = do
  vm <- State.get
  let
    p = prog vm
    p' = setAddress p address value
  State.put vm{prog = p'}

readFrom :: Int -> State VMState Int
readFrom address = do
  vm <- State.get
  let
    p = prog vm
    v = p ! address
  return . seq v $ v -- Ensure that memory accesses are validated at this point

getBasePtr :: State VMState Int
getBasePtr = do
  vm <- State.get
  return $ basePtr vm

setBasePtr :: Int -> State VMState ()
setBasePtr base = do
  vm <- State.get
  State.put vm{basePtr = base}
  return ()

makeResult :: State VMState Result
makeResult = do
  vm <- State.get
  return Result {
    left = (prog vm) ! 0,
    -- TODO populate this
    output = []
  }

-- Get the value for an input operand
evalInputOperand :: Decoder.Operand -> State VMState Int
evalInputOperand (Decoder.Immediate v) = return v
evalInputOperand (Decoder.Positional address) = do readFrom address
evalInputOperand (Decoder.Relative offset) = do
  base <- getBasePtr
  readFrom (base + offset)

evalDestOperand :: Decoder.Operand -> State VMState Int
evalDestOperand (Decoder.Immediate _) = error "Destination operand cannot be immediate"
evalDestOperand (Decoder.Positional address) = return address
evalDestOperand (Decoder.Relative offset) = do
  base <- getBasePtr
  return (base + offset)

initialVMState :: Program -> VMState
initialVMState p = VMState {
    ip = 0,
    basePtr = 0,
    prog = p,
    input = Nothing
  }

setInput :: Int -> State VMState ()
setInput v = do
  vm <- State.get
  State.put vm{input = (Just v)}

consumeInput :: State VMState (Maybe Int)
consumeInput = do
  vm <- State.get
  State.put vm {input = Nothing}
  return . input $ vm

getBinaryOpImpl :: Decoder.BinOp -> (Int -> Int -> Int)
getBinaryOpImpl Decoder.Add  = (+)
getBinaryOpImpl Decoder.Mult = (*)
getBinaryOpImpl Decoder.LessThan = (\l r -> if l < r then 1 else 0)
getBinaryOpImpl Decoder.Equal = (\l r -> if l == r then 1 else 0)

getJumpTest :: Decoder.JmpWhen -> (Int -> Bool)
getJumpTest Decoder.NonZero = (0/=)
getJumpTest Decoder.Zero = (0==)


incrementIp :: Int -> State VMState ()
incrementIp amount = do
  vm <- State.get 
  let ip' = (ip vm) + amount
  State.put vm{ip = ip'}
  return ()

setIp :: Int -> State VMState ()
setIp to = do
  vm <- State.get 
  State.put vm{ip = to}
  return ()

-- Execute given instruction, return any output produced
execute :: Decoder.Instruction -> State VMState (Maybe Suspended)

execute Decoder.Halt = return (Just Halted)

execute (Decoder.Binary op opA opB opD) = do
  valA <- evalInputOperand opA
  valB <- evalInputOperand opB
  valD <- evalDestOperand opD
  let
    impl = getBinaryOpImpl op
    computed = impl valA valB

  writeTo valD computed
  incrementIp 4
  return Nothing

execute (Decoder.OneOp Decoder.Output operand) = do
  val <- evalInputOperand operand
  incrementIp 2
  return (Just $ Output val)

execute (Decoder.OneOp Decoder.Input opD) = do
  maybeInput <- consumeInput
  case maybeInput of
    (Just val) -> do
      valD <- evalDestOperand opD
      writeTo valD val
      incrementIp 2
      return Nothing

    -- If there is no input, suspend and wait for more input. the IP won't have
    -- been updated so this instruction will execute again but this time the
    -- input value will be available
    (Nothing) -> return (Just InputRequested)

execute (Decoder.OneOp Decoder.ChangeBasePtr opA) = do
  val <- evalInputOperand opA
  base <- getBasePtr
  setBasePtr (base + val)
  incrementIp 2
  return Nothing

execute (Decoder.Jmp when opCond opTo) = do
  val <- evalInputOperand opCond
  let check = getJumpTest when

  if check val then
    do
      target <- evalInputOperand opTo
      setIp target
  else
    incrementIp 3

  return Nothing
  
fetchDecode :: State VMState Decoder.Instruction
fetchDecode =
  do
    vm <- State.get
    let p = prog vm
    -- Get the values for (potentially) the longest instruction, lazy evaluation
    -- means that the list will always be the right length (even if the extra
    -- elements cause an error if ever evaluated). This allows a HALT at the end
    -- of a program to be handled in the same way as a full size instruction
    return $ Decoder.decode $ [p ! ((ip vm) + offset) | offset <- [0..Decoder.longestInstruction-1]]

-- Run program until output or halt
step :: State VMState Suspended
step = do
  inst <- fetchDecode
  result <- execute inst
  case result of
    -- Got an output, suspend VM
    Nothing -> step
    (Just suspend) -> return suspend

simpleRun :: [Int] -> ([Int] -> [Int]) -> State VMState Result
simpleRun inputs prevOutput = do
  suspend <- step
  case suspend of
    (Output v) -> simpleRun inputs (prevOutput . (v:))

    InputRequested ->
      do
        case inputs of
          (i:rest) ->
            do
              setInput i
              simpleRun rest (prevOutput)

          _ -> error "Requested input but none left for simple run"

    Halted -> do
      rc <- readFrom 0
      return Result{left = rc, output = (prevOutput []) }

-- Start a VM and run it until it suspends
startVM :: Program -> (Suspended, VMState)
startVM p = State.runState step (initialVMState $ p)

-- Resume a VM previously started with startVM
resumeVM :: (Suspended, VMState) -> (Suspended, VMState)
resumeVM ((Halted), _) = error "Cannot resume halted VM"
resumeVM (_, state) = State.runState step state

resumeVMWithInput :: Int -> (Suspended, VMState) -> (Suspended, VMState)
resumeVMWithInput _ ((Halted), _) = error "Cannot resume halted VM"
resumeVMWithInput i (_, state) = do
  State.runState step state{input = (Just i)}

run :: Program -> Result
run p = runWithInput [] p

runWithInput :: [Int] -> Program -> Result
runWithInput i p = State.evalState (simpleRun i id) (initialVMState $ p)
