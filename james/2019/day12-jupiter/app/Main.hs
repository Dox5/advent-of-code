module Main where

import Lib
import Parse

import Text.ParserCombinators.ReadP (ReadP, char, endBy, eof, readP_to_S)

import qualified Control.Monad as M
import qualified Control.Monad.Trans.State as State

parsePlanets :: ReadP Planets
parsePlanets = do
  ps <- endBy readVector (char '\n')
  eof
  return $ fmap (\pos -> Planet pos (0, 0, 0)) ps
  

main :: IO ()
main = do
  ipt <- readFile "input.txt"
  let (planets, ""):[] = readP_to_S parsePlanets ipt

  putStrLn "System energy after 1000 steps:"

  let energy = State.evalState (M.replicateM 1000 step >> systemEnergy) planets
  putStrLn $ show energy

