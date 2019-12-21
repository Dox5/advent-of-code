module Lib where

import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP

import Data.Array (Array)
import qualified Data.Array as Array

import qualified Data.Set as Set
import qualified Data.List as List

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Data.Function (on)

type Position = (Int, Int)

data Tile = Empty | Asteroid
  deriving (Show, Eq)

data SpaceMap = SpaceMap {
    asteroidMap :: Array (Int, Int) Tile
  }
  deriving (Show)

type Vector = (Float, Float)
type Angle = Float
type Distance = Float

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
fixFloatingPointError bad = (fromInteger $ (round (bad * 1000))) / 1000

vectorLength :: Vector -> Float
vectorLength (x, y) = sqrt $ x*x + y*y 

normalise :: Vector -> Vector
normalise v@(x, y) = 
  let
    factor = 1/vectorLength v
  in (fixFloatingPointError $ x*factor, fixFloatingPointError $ y *factor)

dotProduct :: Vector -> Vector -> Float
dotProduct (ax, ay) (bx, by) = ax*bx + ay*by

angleBetween :: Vector -> Vector -> Float
angleBetween a b =
  let
    aLen = vectorLength a
    bLen = vectorLength b
  in fixFloatingPointError $ acos $ (dotProduct a b) / (aLen * bLen)

anglesToAsteroids :: SpaceMap -> Position -> [(Position, (Angle, Distance))]
anglesToAsteroids sm origin =
  let
    north = (0.0, -1.0)
    calcAngleDist = (\p ->
      let
        v@(x, _) = vector origin p
        -- Angle is always the _smallest_ between vectors, we want clockwise from
        -- `north` so when in the negative x-plane add pi (half circle)
        distance = vectorLength v
        smallestAngle = angleBetween north v
        bearing = if x < 0 then 2*pi - smallestAngle else smallestAngle
      in (bearing, distance))
  in [(asteroid, calcAngleDist asteroid)
                              | asteroid <- allAsteroids sm, asteroid /= origin]

laserPasses :: [(Position, (Angle, Distance))] -> HashMap Float [(Position)]
laserPasses = fmap (fmap fst . List.sortBy (compare `on` snd)) . List.foldl' f HashMap.empty
  where
    f :: HashMap Float [(Position, Distance)] -> (Position, (Angle, Distance)) -> HashMap Float [(Position, Distance)]
    f h (p, (angle, dist)) = HashMap.alter (addToList (p, dist)) angle h

    -- Note: This inverts the list!
    addToList :: a -> Maybe [a] -> Maybe [a]
    addToList p Nothing = Just $ p:[]
    addToList p (Just ps) = Just $ p:ps

vector :: Position -> Position -> Vector
vector (x1, y1) (x2, y2) = (fromIntegral $ x2 - x1, fromIntegral $ y2 - y1)

-- Consume each list 1 element at a time until they are all empty
interleave :: [[a]] -> [a]
interleave = List.concat . List.transpose

countVisableAsteroids :: SpaceMap -> [(Position, Int)]
countVisableAsteroids sm =
  let
    asteroids = allAsteroids sm
    sightLines = [(from, [v | to <- asteroids, from /= to, let v = normalise $ vector from to]) | from <- asteroids]
  in fmap (\(p, v) -> (p, Set.size . Set.fromList $ v)) sightLines

destructionOrder :: SpaceMap -> Position -> [Position]
destructionOrder sm laser =
  let
    angles = anglesToAsteroids sm laser
    asteroidGroups = HashMap.foldrWithKey (\k v a -> (k, v):a) [] $ laserPasses angles
    orderedGroups = fmap snd $ List.sortBy (compare `on` fst) asteroidGroups
  in interleave orderedGroups
