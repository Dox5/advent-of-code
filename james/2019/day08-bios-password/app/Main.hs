module Main where

import qualified System.IO as IO

import Lib

getInput :: IO Image
getInput = do
  contents <- IO.readFile "input.txt"
  return $ loadSpaceImage 25 6 contents

main :: IO ()
main = do
  img <- getInput
  putStrLn . showString "Checksum: " . show . checksum $ img
  putStrLn "Rendered:"
  putStrLn . render . rasterise $ img
