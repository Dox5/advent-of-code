-- Provides main interface to VM to execute programs
module IntCode (
  Program,

  Result,
  left,
  output,

  Suspended(Halted, Output, InputRequested),

  VMState,
  load,
  run,
  runWithInput,
  startVM,
  resumeVM,
  resumeVMWithInput
)
where

import IntCode.Executer
import IntCode.Program
