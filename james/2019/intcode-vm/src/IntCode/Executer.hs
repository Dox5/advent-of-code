module IntCode.Executer where

import qualified IntCode.Decoder as Decoder
import IntCode.Program

import Control.Monad.State (State)
import qualified Control.Monad.State as State

-- Holds the current execution state
data VmState = VmState {
  halt :: Bool,
  ip :: Int
}
  deriving (Show)

data Result = Result {
  -- Left most int result
  left :: Int
}
  deriving (Show)

initialVmState = VmState False 0

getBinaryOpImpl :: Decoder.BinOp -> (Int -> Int -> Int)
getBinaryOpImpl Decoder.Add  = (+)
getBinaryOpImpl Decoder.Mult = (*)

incrementIp :: Int -> VmState -> VmState
incrementIp amount (VmState h ip) = VmState h (ip + amount)

setHalt :: VmState -> VmState
setHalt (VmState _ ip) = VmState True ip

execute :: Decoder.Instruction -> Program -> VmState -> (VmState, Program)

execute Decoder.Halt p vm = (setHalt vm, p)

execute (Decoder.Binary op (Decoder.Positional a) (Decoder.Positional b) (Decoder.Positional dest)) p vm =
  let
    impl = getBinaryOpImpl op
    valA = p ! a
    valB = p ! b
    computed = impl valA valB
  in (incrementIp 4 vm, (writeTo p dest computed))

--execute _ _ _ = error "Illegal instruction"

fetchDecode :: VmState -> Program -> Decoder.Instruction
fetchDecode vm p =
  let
    -- Get the values for (potentially) the longest instruction
    -- TODO: Stop this breaking for short instructions (such as HALT
    codes = [p ! ((ip vm) + offset) | offset <- [0..Decoder.longestInstruction-1]]
  in Decoder.decode codes

step :: Program -> State VmState Result
step prog = do
  vm <- State.get
  if halt vm then
    return . Result . head . makeResult $ prog
  else
    do
      let (vm', prog') = execute (fetchDecode vm prog) prog vm
      State.put vm'
      step prog'

run :: Program -> Result
run p = State.evalState (step p) initialVmState
