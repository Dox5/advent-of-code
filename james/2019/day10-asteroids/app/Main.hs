module Main where

import Lib

import qualified System.IO as IO
import qualified System.Environment as Env
import qualified Data.List as List

import Data.Function (on)

readMapFile :: IO.FilePath -> IO SpaceMap
readMapFile path = do
  contents <- IO.readFile path
  return . parseMap $ contents

getPath :: IO IO.FilePath
getPath = do
  (p:_) <- Env.getArgs
  return p

canSeeMost :: SpaceMap -> (Position, Int)
canSeeMost = List.maximumBy (compare `on` snd) . countVisableAsteroids

main :: IO ()
main = do
  path <- getPath
  spaceMap <- readMapFile path
  putStrLn . showString "Best view: " $ show (canSeeMost spaceMap)
