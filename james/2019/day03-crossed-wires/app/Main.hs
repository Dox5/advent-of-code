module Main where

import Lib

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List

import qualified System.IO as IO


closestIntersection :: Point -> [Path] -> Int
closestIntersection to paths =
  let
    candidates = intersections (fmap touched paths)
  in foldr1 min . fmap (manhattenDistance to) $ candidates

combinedDelay :: [PropDelay] -> Point -> Maybe Int
combinedDelay lookups p =
  let
    delays = fmap (\l -> HashMap.lookup p l) lookups
  in fmap sum $ sequence delays

leastDelay :: [Path] -> Maybe Int
leastDelay paths =
  let
    candidates = intersections $ fmap touched paths
    pointDelayLookup = fmap calcPropDelay paths
    propDelays = fmap (combinedDelay pointDelayLookup) $ candidates
  in fmap (List.foldl1' min) $ sequence propDelays


wirePaths :: IO([Path])
wirePaths = do
  contents <- IO.readFile "paths.txt"
  return . fmap walkPath . lines $ contents

  

main :: IO ()
main = do
  paths <- wirePaths
  putStrLn . showString "Closest: " . show . closestIntersection (Point 0 0) $ paths
  putStrLn . showString "Least delay: " . show . leastDelay $ paths 
