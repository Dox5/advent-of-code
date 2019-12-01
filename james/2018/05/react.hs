module Main(main) where

import qualified Data.Maybe as Maybe
import qualified Data.Either as Either
import qualified Data.Char as Char
import qualified Data.List as List
import qualified System.IO as IO

halves :: [a] -> ([a], [a])
halves lst = List.splitAt halfway lst
  where
    halfway = length lst `div` 2

type Molecule = Char
type Polymer  = [Molecule]
type PolymerS = Polymer -> Polymer

-- Are the two molecules going to destroy each other
destructive :: Molecule -> Molecule -> Bool
destructive m1 m2 = sameChar m1 m2 && oppositeCase m1 m2
  where
    sameChar l r = (==) (Char.toUpper l) (Char.toUpper r)
    oppositeCase l r = List.any id [Char.isUpper l && Char.isLower r,
                                    Char.isUpper r && Char.isLower l]

react :: Molecule -> Molecule -> PolymerS
react l r = if destructive l r then id else ((l:r:[]) ++) 

zipKeepExtra :: [a] -> [b] -> ([(a, b)], Maybe.Maybe (Either.Either [a] [b]))
zipKeepExtra l r 
  | (length l) < (length r)  = (zip l r, Maybe.Just (Either.Right $ drop (length l) r))
  | (length l) > (length r)  = (zip l r, Maybe.Just (Either.Left $ drop (length r) l))
  | otherwise = (zip l r, Maybe.Nothing)

-- Take two reacted polymers and combine them (in the order they appear as
-- arguments) resolving any reactions the comibination causes
combines :: Polymer -> Polymer -> PolymerS
combines l r = leftPoly . rightPoly
  where
    zipped = zipKeepExtra (reverse l) r
    linedUp = fst $ zipped
    remainder = snd $ zipped
    reacted = dropWhile (uncurry destructive) linedUp
    splitUp = unzip reacted
    reactedLeft  = reverse . fst $ splitUp
    reactedRight = snd $ splitUp

    extraRight :: Maybe (Either.Either Polymer Polymer) -> Polymer
    extraRight (Just (Right p)) = p
    extraRight _ = []

    extraLeft :: Maybe (Either.Either Polymer Polymer) -> Polymer
    extraLeft (Just (Left p)) = reverse $ p
    extraLeft _ = []

    leftPoly  = ((extraLeft remainder) ++) . (reactedLeft ++)
    rightPoly = (reactedRight ++) . ((extraRight remainder) ++)


-- Split a into a tree and react recursively
reactFully :: Polymer -> Polymer
reactFully polymer = reactFullys polymer $ []
  where
    recurse :: Polymer -> Polymer -> PolymerS
    -- Leaf node
    recurse (l:[]) (r:[]) = react l r
    recurse (l:[]) rs     = combines (l:[]) (reactFullys rs $ [])
    recurse ls (r:[])     = combines (reactFullys ls $ []) (r:[])
    recurse ls rs         = combines (reactFullys ls $ []) (reactFullys rs $ [])

    reactFullys :: Polymer -> PolymerS
    reactFullys p = uncurry recurse . halves $ p


-- Get all unique molecules in the given polymer. Ignores polarity
-- All molecules are reported in lower polarity
uniqueMolecules :: Polymer -> [Molecule]
uniqueMolecules polymer = map head $ List.group . List.sort . map Char.toLower $ polymer

-- Remove all instances of a given molecule (either polarity) from a polymer
removeMolecule :: Molecule -> Polymer -> Polymer
removeMolecule m polymer = filter moleculeIgnorePolatiry polymer
  where
    moleculeIgnorePolatiry :: Molecule -> Bool
    moleculeIgnorePolatiry haystack = (Char.toLower haystack) /= normalMole
      where
        normalMole = Char.toLower(m)

-- Returns a list of polymers with which molecule was removed
candidates :: Polymer -> [(Polymer, Molecule)]
candidates polymer = [(removeMolecule m polymer, m) | m <- moles]
  where
    moles = uniqueMolecules polymer

-- Calculates the 'score' of a polymer
calculateScore :: Polymer -> Int
calculateScore polymer = length polymer

-- Score a list of polymers
scoreCandidates :: [Polymer] -> [Int]
scoreCandidates polymers = map calculateScore polymers

reactCanidates :: [Polymer] -> [Polymer]
reactCanidates polymers = map reactFully polymers

-- Find the best molecule to remove to get the biggest reaction (most reduction
-- in size)
bestMoleculeRemoval :: Polymer -> (Molecule, Int)
bestMoleculeRemoval polymer = List.minimumBy score . zip removedMole $ scores
  where
    c = unzip . candidates $ polymer
    removedMole = snd c
    canidatePoly = reactCanidates . fst $ c
    scores = scoreCandidates canidatePoly
    score l r = compare (snd l) (snd r)

getPolymer :: IO.FilePath -> IO String
getPolymer path = do
  contents <- IO.readFile path
  return . head . lines $ contents


main :: IO ()
main = do
  let fileName = "input.txt"
  polymer <- getPolymer fileName
  let reactedPolymer = reactFully polymer
  let bestRemoval = bestMoleculeRemoval polymer
  putStrLn . showString "Reacted Polymer: " . showString reactedPolymer $ ""
  putStrLn . showString "[" . (shows $ length reactedPolymer) . showString " molecules long]" $ ""
  putStrLn . showString "Removing the molecule " . (shows . fst $ bestRemoval) . showString " reduces polymer to " . (shows . snd $ bestRemoval) . showString " molecules long" $ ""

