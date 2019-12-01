module Main(main) where

import Data.Function (on)
import Data.Ord (comparing)

import qualified Data.List as List
import qualified Data.Set as Set
import qualified System.IO as IO

data Point = Point Int Int deriving(Eq,Ord)

instance Show Point where
  showsPrec _ (Point x y) = ("(" ++) . shows x . (", " ++) . shows y . (")" ++)

instance Read Point where
  readsPrec _ s = [(Point x y, remaining)
    | (x, r1)  <- reads s,
      (",", r2) <- lex r1,
      (y, remaining) <-reads r2]

pointX :: Point -> Int
pointX (Point x _) = x

pointY :: Point -> Int
pointY (Point _ y) = y

-- Bounds of a single dimension, represented as [min, max)
data Bound = Bound Int Int

instance Show Bound where
  showsPrec _ (Bound min max) = ("[" ++) . shows min . (", " ++) . shows max . (")" ++)

withinBound :: Bound -> Int -> Bool
withinBound (Bound min max) val = min <= val && val < max

newtype Identifier = Identifier Int deriving(Eq, Ord)

instance Show Identifier where
  showsPrec _ (Identifier i) = ("#" ++) . shows i

type Score = Int

data Claim = Claim Identifier Point Score | MultiClaim [Identifier] Point Score deriving(Show)

claimPoint :: Claim -> Point
claimPoint (Claim _ point _) = point
claimPoint (MultiClaim _ point _) = point

claimIdent :: Claim -> Identifier
claimIdent (Claim ident _ _) = ident

-- Collect the identifers of all claims into a single list
collectIdents :: [Claim] -> [Identifier]
collectIdents claims = unique . List.concatMap collect $ claims
  where
    collect :: Claim -> [Identifier]
    collect (Claim ident _ _) = [ident]
    collect (MultiClaim idents _ _) = idents

    unique = map (head) . List.group . List.sort

claimScore :: Claim -> Score
claimScore (Claim _ _ score) = score
claimScore (MultiClaim _ _ score) = score

landingPoints :: IO.FilePath -> IO [Point]
landingPoints path = do
  contents <- IO.readFile path
  return . map read . lines $ contents

makeInitialClaims :: [Point] -> [Claim]
makeInitialClaims points = map (uncurry makeClaim) . zip points $ [0..]
  where
    makeClaim :: Point -> Int -> Claim
    makeClaim point ident = Claim (Identifier ident) point 0

dimensionBound ::(Point -> Int) -> [Point] -> Bound
dimensionBound getDim points = Bound min (max + 1)
  where
    min = getDim . List.minimumBy (comparing getDim) $ points
    max = getDim . List.maximumBy (comparing getDim) $ points
  

-- Work out the width of the claim area [min, max), returns max
width :: [Point] -> Bound
width = dimensionBound pointX

height :: [Point] -> Bound
height = dimensionBound pointY

neighbours :: Point -> [Point]
neighbours (Point x y)
  = [(Point (x+xOffset) (y+yOffset)) | (xOffset, yOffset) <- cardinals]
  where 
    cardinals = [(-1, 0), (1, 0), (0, -1), (0, 1)]

-- Claim all cells in the cardinial directions, using a higher score
claimNeighbours :: Claim -> [Claim]
claimNeighbours (Claim ident point score) = map makeClaim . neighbours $ point
  where
    makeClaim :: Point -> Claim
    makeClaim p = Claim ident p (score+1)

claimNeighbours (MultiClaim idents point score) = map makeMultiClaim . neighbours $ point
  where
    makeMultiClaim :: Point -> Claim
    makeMultiClaim p = MultiClaim idents point (score+1)

-- Combine any claims for the same cell into a multiclaim
combineClaims :: [Claim] -> [Claim]
combineClaims flatClaims = map combine . groupAndsortByPoints $ flatClaims
  where
    groupAndsortByPoints = List.groupBy (on (==) claimPoint) . List.sortBy (comparing claimPoint)
    combine :: [Claim] -> Claim
    combine cs = makeClaim (collectIdents cs) (claimPoint . head $ cs) (claimScore . head $ cs)
      where
        makeClaim :: [Identifier] -> Point -> Score -> Claim
        makeClaim (ident:[]) point score = Claim ident point score
        makeClaim idents point score     = MultiClaim idents point score
    
    

-- Takes set of claims and removes that already exist in the list of points
dropClaimed :: Set.Set Point -> [Claim] -> [Claim]
dropClaimed claimed claims = filter (not . (flip (Set.member) claimed) . claimPoint) claims


type Generation = [Claim]

-- List of prevous generations, oldest (lowest number) being on the far right
-- Makes a new generation
nextGeneration :: Set.Set Point -> [Generation] -> Generation
nextGeneration claimed (lastGen:_)
  = dropClaimed claimed . combineClaims . List.concatMap claimNeighbours $ lastGen

dropOutOfBounds :: (Bound, Bound) -> Generation -> Generation
dropOutOfBounds bounds gen = filter inBounds gen
  where
    inBounds claim = pointInBounds bounds (claimPoint claim)
    pointInBounds :: (Bound, Bound) -> Point -> Bool
    pointInBounds (xBound, yBound) (Point x y) = (withinBound xBound x) && (withinBound yBound y)


-- Workout all the closests points to every landing point
landingPointAreas :: (Bound, Bound) -> [Claim] -> [Claim]
landingPointAreas bounds initial = f Set.empty [initial]
  where
    -- This function doesn't deserve a real name, it's horrible.
    f :: Set.Set Point -> [Generation] -> [Claim]
    f claimed ([]:gens) = List.concat gens
    f claimed gens = f (foldr Set.insert claimed (map claimPoint next)) (next:gens)
      where
        next = dropOutOfBounds bounds . nextGeneration claimed $ (gens)

touchesBorder :: (Bound, Bound) -> Claim -> Bool
touchesBorder bounds claim = pointOnEdge bounds (claimPoint claim)
  where
    pointOnEdge :: (Bound, Bound) -> Point -> Bool
    pointOnEdge (xBound, yBound) (Point x y) = (touches xBound x) || (touches yBound y)

    touches :: Bound -> Int -> Bool
    touches (Bound min max) val = (val == min) || (val == (max - 1))

finiteClaims :: (Bound, Bound) -> [Claim] -> [Claim]
finiteClaims bounds claims = filter (not . touchesBorder bounds) claims

noMultiClaims :: [Claim] -> [Claim]
noMultiClaims claims = filter (not . isMulti) claims
  where
    isMulti :: Claim -> Bool
    isMulti (Claim _ _ _) = False
    isMulti (MultiClaim _ _ _) = True

biggestFiniteArea :: (Bound, Bound) -> [Claim] -> (Identifier, Int)
biggestFiniteArea bounds claims = toIdentLen . biggestGroup $ claims
  where
    validClaims  = noMultiClaims . finiteClaims bounds
    areasByIdent = List.groupBy (on (==) claimIdent) . List.sortBy (comparing claimIdent)
    biggestGroup = List.maximumBy (comparing length) . areasByIdent . validClaims
    toIdentLen claims = ((claimIdent . head $ claims), length claims)

manhattenDistance :: Point -> Point -> Int
manhattenDistance p1 p2 = (dimDist pointX) + (dimDist pointY)
  where
    dimDist getDim = abs ((getDim p1) - (getDim p2))

-- Take a list of points and check that the given single point is less than the
-- specified distance away from all points
distanceToAllLessThan :: [Point] -> Int -> Point -> Bool
distanceToAllLessThan allPoints maxDistSum p = f allPoints 0
  where
    f ([]) total = total < maxDistSum
    f (toPoint:toPoints) total =
      if newTotal >= maxDistSum then False else f toPoints newTotal
      where
        newTotal = (total +) $ manhattenDistance toPoint p

safeLandingArea :: [Point] -> [Point]
safeLandingArea givenCoords = filter (distanceToAllLessThan givenCoords maxDist) everyCell
  where
    maxDist = 10000
    everyCell = cells (width givenCoords, height givenCoords)

cells :: (Bound, Bound) -> [Point]
cells ((Bound xMin xMax),(Bound yMin yMax))
  = [Point x y | x <- [xMin..(xMax-1)], y <-[yMin..(yMax-1)]]

part1 :: IO ()
part1 = do
  let input = "input.txt"
  initialPoints <- landingPoints input
  let initialClaims = makeInitialClaims initialPoints
  let bounds = (width initialPoints, height initialPoints)
  let areas = landingPointAreas bounds initialClaims

  putStrLn . showString "Best landing site: " . shows (biggestFiniteArea bounds areas) $ ""

part2 :: IO ()
part2 = do
  let input = "input.txt"
  initialPoints <- landingPoints input
  putStrLn ""

main :: IO ()
main = do
  part1
