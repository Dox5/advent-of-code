module Main(main) where

import System.IO

filterPlus :: String -> String
filterPlus str = filter (/='+') str

getFreqAdjustments :: FilePath -> IO [Integer]
getFreqAdjustments path = do
  contents <- readFile path
  let adjustments = map (read . filterPlus) $ lines $ contents
  return adjustments


main :: IO ()
main = do
  let fileName = "input.txt"
  allInputs <- getFreqAdjustments fileName
  putStrLn . show . sum  $ allInputs

