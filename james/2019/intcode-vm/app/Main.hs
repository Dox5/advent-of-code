{-# LANGUAGE DeriveDataTypeable #-}

import qualified IntCode as IntCode

-- I don't like this commandline parser but I'v already spent too long looking
-- for a good one
import qualified System.Console.CmdArgs as CmdArgs
import System.Console.CmdArgs ((&=))

import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP

import qualified System.IO as IO

data Args = Args {
    inputFile   :: IO.FilePath,
    programFile :: IO.FilePath
}
  deriving (Show, CmdArgs.Typeable, CmdArgs.Data)

comma :: ReadP Char
comma = ReadP.char  ','

digit :: ReadP Char
digit = ReadP.satisfy (\c -> any (c==) "0123456789")

negativeNumber :: ReadP Int
negativeNumber = do
  ReadP.skipSpaces
  _ <- ReadP.char '-'
  num <- ReadP.many1 digit
  return $ read ('-':num)

number :: ReadP Int
number = do
  ReadP.skipSpaces
  num <- ReadP.many1 digit
  return $ read num

numbers :: ReadP [Int]
numbers = ReadP.sepBy (ReadP.choice [number, negativeNumber]) comma

inputs :: ReadP [Int]
inputs = do
  is <- numbers
  ReadP.optional (ReadP.char '\n')
  ReadP.eof
  return is


intcode :: ReadP IntCode.Program
intcode = do
  m <- numbers
  ReadP.optional (ReadP.char '\n')
  ReadP.eof
  return $ IntCode.load m

loadFromFile :: IO.FilePath -> IO IntCode.Program
loadFromFile path = do
  contents <- IO.readFile path
  let parsed = ReadP.readP_to_S intcode $ seq contents contents

  case parsed of
    ((p, ""):[]) -> return p
    _ -> error $ "Failed to parse program"

loadInputFromFile :: IO.FilePath -> IO [Int]
loadInputFromFile "" = return []
loadInputFromFile path = do
  contents <- IO.readFile path
  let parsed = ReadP.readP_to_S inputs $ seq contents contents

  case parsed of
    ((p, ""):[]) -> return p
    _ -> error $ "Failed to parse inputs"

intcodeArgs :: Args
intcodeArgs = Args {
  inputFile = CmdArgs.def 
    &= CmdArgs.opt ""
    &= CmdArgs.typFile
    &= CmdArgs.help "File with comma separated ints to be provided when input requsted",

  programFile = CmdArgs.def 
    &= CmdArgs.argPos 0
    &= CmdArgs.typFile
}


main :: IO()
main = do
  args <- CmdArgs.cmdArgs intcodeArgs
  prog <- loadFromFile (programFile args)
  ipt  <- loadInputFromFile (inputFile args)

  putStrLn "Executing machine..."
  let result = IntCode.runWithInput ipt prog
  putStrLn $ show result
