module Main(main) where

import qualified System.IO as IO
import qualified Data.List as List
import qualified Data.Tuple as Tuple

getBoxIds :: IO.FilePath -> IO [String]
getBoxIds path = do
  contents <- IO.readFile path
  return $ lines $ contents

countIf :: (a -> Bool) -> [a] -> Int
countIf pred as = sum $ map (trueIsOne . pred) as
  where
    trueIsOne :: Bool -> Int
    trueIsOne True  = 1
    trueIsOne False = 0

-- Strings MUST be same length
hammingDistance :: String -> String -> Int
hammingDistance l r = countIf (==False) . map sameCharacter $ zip l r
  where
    sameCharacter (x, y) = x == y
    
candidates :: [String] -> [(String, String)]
candidates all = workhorse all []
  where
    workhorse :: [String] -> [(String, String)] -> [(String, String)]
    workhorse [] foundCandidates          = foundCandidates
    workhorse (test:rest) foundCandidates = workhorse rest (foundCandidates ++ addCandidates)
      where
        addCandidates :: [(String, String)]
        addCandidates = map makeCandidatePair . filter distanceIsOne $ map (hDist test) rest

        hDist :: String -> String -> (Int, String)
        hDist a b = (hammingDistance a b, b)
      
        makeCandidatePair :: (Int, String) -> (String, String)
        makeCandidatePair (_, s) = (test, s)

        distanceIsOne :: (Int, String) -> Bool
        distanceIsOne d _ = d == 1


main :: IO ()
main = do
 let file = "input.txt"
 ids <- getBoxIds file
 putStrLn . show . map (uncurry List.intersect) $ candidates ids
