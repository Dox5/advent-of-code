module Main where

import Lib

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

countIf :: (a -> Bool) -> [a] -> Int
countIf f = sum . fmap (boolToInt . f)

inRange :: [Int]
inRange = [158126..624574]

main :: IO ()
main = do
  putStrLn . shows (countIf potentialPassword inRange) . showString " potential passwords" $ ""
  putStrLn . shows (countIf potentialPassword2 inRange) . showString " potential passwords (stonger requirements)" $ ""
