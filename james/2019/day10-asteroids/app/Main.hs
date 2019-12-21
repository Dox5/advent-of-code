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

nthDestroyed :: Int -> Position -> SpaceMap -> Position
nthDestroyed n base sm = head . drop (n-1) . destructionOrder sm $ base

main :: IO ()
main = do
  path <- getPath
  spaceMap <- readMapFile path
  let (base, visableCount) = canSeeMost spaceMap
  let betOn = nthDestroyed 200 base spaceMap
  putStrLn . showString "Best view: " . shows base
    . shows ", can see: " . shows visableCount . show $ " asteroids"

  putStrLn . showString "Bet on: " . show $ betOn
