module Main where

import qualified System.IO as IO

import Lib

getComponentWeights :: IO.FilePath -> IO ([Int])
getComponentWeights path = do
  contents <- IO.readFile path
  return . map read . lines $ contents

fuelRequired :: [Int] -> Int
fuelRequired = sum . map calcFuelRequirement

fuelRequiredCompound :: [Int] -> Int
fuelRequiredCompound = sum . map calcCompoundFuelRequirement

main :: IO ()
main = do
  weights <- getComponentWeights "weights.txt"
  putStrLn . showString "Total fuel required:            " . shows (fuelRequired weights) $ ""
  putStrLn . showString "Total fuel required (compound): " . shows (fuelRequiredCompound weights) $ ""
