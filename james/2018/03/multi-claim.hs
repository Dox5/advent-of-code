module Main(main) where

import qualified Data.List as List
import qualified System.IO as IO

data Point a = Pt a a deriving (Show,Ord,Eq)
data Size  a = Sz a a deriving (Show)

data Box a =  Bx (Point a) (Size a) deriving (Show)

data Claim = Clm Int (Box Int) deriving (Show)

instance Read a => Read (Point a) where
  readsPrec _ s = [(Pt x y, remaining) | (x, t) <- reads s,
                                         (",", u) <- lex t,
                                         (y, remaining) <- reads u ]

instance Read a => Read (Size a) where
  readsPrec _ s = [(Sz w h, remaining) | (w, 'x':t) <- reads s,
                                         (h, remaining) <- reads t ]

instance Read Claim where
  readsPrec _ s = [(Clm id $ Bx point size, remaining) | ("#", t)   <- lex s,
                                                        (id, u)    <- reads t,
                                                        ("@", v)   <- lex u,
                                                        (point, w) <- reads v,
                                                        (":", x)   <- lex w,
                                                        (size, remaining) <- reads x]

type IdAndPoint = (Int, Point Int)

grabPoint :: IdAndPoint -> Point Int
grabPoint (_, p) = p

getClaims :: IO.FilePath -> IO [Claim]
getClaims path = do
  contents <- IO.readFile path
  return . map (fst . head . reads) . lines $ contents

-- Make a Point for every cell that this box covers
boxToCells :: (Num a, Enum a) => Box a -> [Point a]
boxToCells (Bx (Pt x y) (Sz w h)) = [Pt cellX cellY | cellX <- [x..xExtent],
                                                      cellY <- [y..yExtent]]
  where
    xExtent = x + w - 1
    yExtent = y + h - 1

areaCellsFromClaims :: [Claim] -> [IdAndPoint]
areaCellsFromClaims claims = concatMap (pointsWithIds . idAndBox)  claims
  where
    idAndBox (Clm id b) = (id, b)
    pointsWithIds :: (Int, Box Int) -> [IdAndPoint]
    pointsWithIds (id, box) = zip (repeat id) (boxToCells box)

-- Takes a list of (claimid point), returns a list of points that overlap with
-- a list of all the claim ids that overlapped with this one
cellsThatOverlap :: [IdAndPoint] -> [([Int], Point Int)]
cellsThatOverlap points = filter moreThanOnePoint . map countAndPoint . groupByPoint . sortByPoint $ points
  where
    countAndPoint points      = (map fst points, snd . head $ points)
    moreThanOnePoint (claimIds, _) =  length claimIds > 1
    sortByPoint :: [IdAndPoint] -> [IdAndPoint]
    sortByPoint = List.sortBy (\l r -> compare (grabPoint l) (grabPoint r))
    groupByPoint :: [IdAndPoint] -> [[IdAndPoint]]
    groupByPoint = List.groupBy (\l r -> (grabPoint l) == (grabPoint r))


claimsThatOverlapped :: [([Int], a)] -> [Int]
claimsThatOverlapped cells = map head . List.group . List.sort . concatMap (\(x,_) -> x) $ cells

allClaimIds :: [Claim] -> [Int]
allClaimIds claims = map grabId claims 
  where
    grabId (Clm id _) = id

main :: IO ()
main = do
  let file = "input.txt"
  claims <- getClaims file
  let overlappingCells = cellsThatOverlap . areaCellsFromClaims $ claims
  putStrLn . shows (length overlappingCells) $ showString " inch^2" ""
  putStrLn . showString "Unique Patch: " . show $ (allClaimIds claims) List.\\ (claimsThatOverlapped overlappingCells)
