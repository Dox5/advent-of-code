-- Provides main interface to VM to execute programs
module IntCode (
  Program,

  Result,
  left,

  load,
  run
)
where

import IntCode.Decoder
import IntCode.Executer
import IntCode.Program
