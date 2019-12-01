module Main(main) where

import System.IO
import Data.Maybe
import Data.List

filterPlus :: String -> String
filterPlus str = filter (/='+') str

getFreqAdjustments :: FilePath -> IO [Int]
getFreqAdjustments path = do
  contents <- readFile path
  let adjustments = map (read . filterPlus) $ lines $ contents
  return adjustments

cumsum :: [Int] -> [Int]
cumsum [] = []
cumsum xs = cumsum_recurse 0 xs
  where
    cumsum_recurse :: Int -> [Int] -> [Int]
    cumsum_recurse _ [] = []
    cumsum_recurse total (x:xs) = newTotal : cumsum_recurse newTotal xs
      where
        newTotal = x + total

firstDuplicate :: (Eq a) => [a] -> Maybe a
firstDuplicate all = workFn [] all
  where
    workFn :: (Eq a) => [a] -> [a] -> Maybe a
    workFn _ [] = Nothing
    workFn seen (x:xs) = if isJust $ find (==x) seen then
        Just x
      else
        workFn (x:seen) xs

main :: IO ()
main = do
  let fileName = "input.txt"
  allInputs <- getFreqAdjustments fileName
  putStrLn $ (show . firstDuplicate . cumsum . cycle) allInputs
