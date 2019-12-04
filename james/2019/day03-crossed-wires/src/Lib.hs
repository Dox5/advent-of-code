module Lib (
  Point (Point),
  Path (Path),
    touched,
  PropDelay,
  walkPath,
  intersections,
  manhattenDistance,
  calcPropDelay
)
  where

import qualified Text.ParserCombinators.ReadP as ReadP

import Data.Set (Set)
import qualified Data.Set as Set

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Data.Hashable (Hashable, hashWithSalt)

import qualified Data.List as List

data Point = Point {
  x    :: Int,
  y    :: Int
}
  deriving (Show, Eq, Ord)

instance Hashable Point where
  hashWithSalt salt (Point x y) = hashWithSalt salt (x, y)

data Direction = Up | Down | Left | Right
  deriving (Show)

data Movement = Movement {
  dir  :: Direction,
  dist :: Int
}
  deriving (Show)

data Path = Path {
  touched :: [Point],
  length  :: Int
}
  deriving (Show)

type PropDelay = HashMap Point Int

comma :: ReadP.ReadP Char
comma = ReadP.char ','

digit :: ReadP.ReadP Char
digit = ReadP.satisfy (\c -> any (c==) "0123456789")

toDirection :: Char -> Direction
toDirection 'U' = Lib.Up
toDirection 'D' = Lib.Down
toDirection 'L' = Lib.Left
toDirection 'R' = Lib.Right

direction :: ReadP.ReadP Direction
direction = do
  c <- ReadP.satisfy (\c -> any (c==) "UDLR")
  return (toDirection c)

distance :: ReadP.ReadP Int
distance = do
  num <- ReadP.many1 digit
  return (read num)

movement :: ReadP.ReadP Movement
movement = do
  ReadP.skipSpaces
  dr <- direction
  di <- distance 
  return (Movement dr di)

movements :: ReadP.ReadP [Movement]
movements = ReadP.sepBy movement comma
      

parseMovements :: String -> [Movement]
parseMovements ipt =
  let
    (parsed, remaining) = last $ ReadP.readP_to_S movements ipt
  in if null remaining then parsed else []

-- Take a start point and workout all the walked points, in reverse order
-- so that the head of the list is the newest point (and the current position)
walkedPoints :: Point -> Movement -> [Point]
walkedPoints (Point x y) (Movement dr di) =
  let
    op = case dr of
           Lib.Up    -> (\(v) -> (x, y + v))
           Lib.Down  -> (\(v) -> (x, y - v))
           Lib.Right -> (\(v) -> (x + v, y))
           Lib.Left  -> (\(v) -> (x - v, y))

    range = [di,(di-1)..1]
  in [uncurry Point (op v) | v <- range]

totalWalkLength :: [Movement] -> Int
totalWalkLength = List.foldl' (sumDist) 0
  where
    sumDist :: Int -> Movement -> Int
    sumDist accum (Movement _ d) = accum + d 

walk :: Point -> [Movement] -> [Point] -> [Point]
walk currLoc [] = ([] ++ )
walk currLoc (movement:moves) =
  let
    (newLoc:points) = walkedPoints currLoc movement
  in ((walk newLoc moves) (newLoc:points) ++)
    

-- Take a string of movements (direction & distance) and return a set of points
walkPath :: String -> Path
walkPath ipt =
  let
    movements = parseMovements ipt
    ps = walk (Point 0 0) movements []
    numSteps = totalWalkLength movements
  in Path ps numSteps

-- Take a number of paths (at least 2!) at return any intersections
intersections :: [[Point]] -> [Point]
intersections [] = []
intersections (x:[]) = [] -- No self intersection
intersections paths =
  let
    pathSets = fmap Set.fromList paths
  in Set.toList . List.foldl1' Set.intersection $ pathSets

-- Path points are in reverse order, always (if not badness!)
calcPropDelay :: Path -> PropDelay
calcPropDelay (Path ps l) =
  let
    propDelay = [l,l-1..1]
    withDelay = zip ps propDelay
  in HashMap.fromListWith min withDelay

manhattenDistance :: Point -> Point -> Int
manhattenDistance (Point ax ay) (Point bx by) = (abs (bx - ax)) + (abs (by - ay))
