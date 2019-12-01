module Main(main) where

import qualified System.IO as IO
import qualified Data.List as List
import qualified Data.Tuple as Tuple

data HasTwoThree = HTT Int Int deriving (Show)



getBoxIds :: IO.FilePath -> IO [String]
getBoxIds path = do
  contents <- IO.readFile path
  return $ lines $ contents
    
groupCharacters :: [String] -> [[String]]
groupCharacters strings = map (List.group . List.sort) $ strings

checkTwosAndThrees :: [Int] -> HasTwoThree
checkTwosAndThrees counts = HTT (findNum 2) (findNum 3)
  where
    findNum :: Int -> Int
    findNum needle = oneIfFound . List.find (==needle) $ counts

    oneIfFound :: Maybe a -> Int
    oneIfFound (Just _)  = 1
    oneIfFound Nothing = 0

splitHTT :: [HasTwoThree] -> ([Int], [Int])
splitHTT htt = unzip . map (httToTuple) $ htt
  where
    httToTuple (HTT twos threes) = (twos, threes)

checksum :: [String] -> Int
checksum ids = twos * threes
  where
    twosAndThrees = splitHTT . map (checkTwosAndThrees . map length) . groupCharacters $ ids 
    twos          = sum . Tuple.fst $ twosAndThrees
    threes        = sum . Tuple.snd $ twosAndThrees

main :: IO ()
main = do
 let file = "input.txt"
 ids <- getBoxIds file
 putStrLn . show . checksum $ ids
