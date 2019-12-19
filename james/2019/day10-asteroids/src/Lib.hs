module Lib where

import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP

import Data.Array (Array)
import qualified Data.Array as Array

import qualified Data.Set as Set

type Position = (Int, Int)

data Tile = Empty | Asteroid
  deriving (Show, Eq)

data SpaceMap = SpaceMap {
    asteroidMap :: Array (Int, Int) Tile
  }
  deriving (Show)

type Vector = (Float, Float)

ix :: [Int]
ix = [0..]

buildMap :: [[Tile]] -> SpaceMap
buildMap tiles =
  let
    height = length tiles
    width = length . head $ tiles 
    bounds = ((0, 0), (width - 1, height - 1))
    indicies = concatMap (\(y, row) -> fmap (\(x, t) -> ((x, y), t)) . zip ix $ row) . zip ix $ tiles
  in SpaceMap $ Array.array bounds indicies

readTile :: ReadP Tile
readTile = do
  c <- ReadP.satisfy (\c -> any (c==) ".#")
  case c of
    '.' -> return Empty
    '#' -> return Asteroid
    _ -> error "Unknown map symbol"

readRow :: ReadP [Tile]
readRow = do
  tiles <- ReadP.many readTile
  _ <- ReadP.char '\n'
  return tiles


readMap :: ReadP [[Tile]]
readMap = do
  tiles <- ReadP.many readRow
  ReadP.eof
  return tiles

parseMap :: String -> SpaceMap
parseMap s = 
  case ReadP.readP_to_S readMap s of
    ((m,""):[]) -> buildMap m
    _ -> error "Parsey-no-worky"

allAsteroids :: SpaceMap -> [Position]
allAsteroids (SpaceMap m) = 
  let
    ((_, _), (w, h)) = Array.bounds m
  in [(x, y) | x <- [0..w], y <-[0..h], (m Array.! (x, y)) == Asteroid] 

-- Fix is a strong term. Reduce the accuracy to make vectors compare equally
fixFloatingPointError :: Float -> Float
fixFloatingPointError bad = (fromInteger $ (round (bad * 10000))) / 10000

normalise :: Vector -> Vector
normalise (x, y) = 
  let
    mag = sqrt $ x*x + y*y
    factor = 1/mag
  in (fixFloatingPointError $ x*factor, fixFloatingPointError $ y *factor)

vector :: Position -> Position -> Vector
vector (x1, y1) (x2, y2) = (fromIntegral $ x2 - x1, fromIntegral $ y2 - y1)

countVisableAsteroids :: SpaceMap -> [(Position, Int)]
countVisableAsteroids sm =
  let
    asteroids = allAsteroids sm
    sightLines = [(from, [v | to <- asteroids, from /= to, let v = normalise $ vector from to]) | from <- asteroids]
  in fmap (\(p, v) -> (p, Set.size . Set.fromList $ v)) sightLines
