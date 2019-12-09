-- Provides main interface to VM to execute programs
module IntCode (
  Program,

  Result,
  left,
  output,

  load,
  run,
  runWithInput
)
where

import IntCode.Executer
import IntCode.Program
