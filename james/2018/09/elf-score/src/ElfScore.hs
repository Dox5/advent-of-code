module ElfScore(main, playGame, playMarble, Elf (Elf), Marble (Marble), Score, Game) where

import Tape

import Data.List (groupBy, sortBy, maximumBy)
import Data.Maybe (fromMaybe)
import Data.Function (on)
import Data.Ord (comparing)

type Score = Int

data Elf = Elf Int deriving (Show, Eq, Ord)

data Marble = Marble Int deriving (Show, Eq)

type Game = Tape Marble

isScoring :: Marble -> Bool
isScoring (Marble v) = (v `rem` 23) == 0

calculateScore :: Marble -> Marble -> Score
calculateScore (Marble a) (Marble b) = (a+b)

playMarble :: Game -> Marble -> (Game, Maybe Score)
playMarble game peice
  | isScoring peice = let
                        fromBoard = current . backward 7 $ game
                        score = Just . calculateScore fromBoard $ peice
                        updatedGame = delete . backward 7 $ game
                      in (updatedGame, score)
  | otherwise       = let
                        score = Nothing
                        updatedGame = forward 1 . insert peice . forward 1 $ game
                      in (updatedGame, score)

foldPlay :: (Game, [Maybe Score]) -> Marble -> (Game, [Maybe Score])
foldPlay (game, scores) marble
  = let
      (updatedGame, thisScore) = playMarble game marble
    in (updatedGame, thisScore:scores)

sumScore :: [Maybe Score] -> Score
sumScore = sum . map (fromMaybe 0)

collateScores :: [(Elf, Maybe Score)] -> [(Elf, Score)]
collateScores = map f . groupBy (on (==) fst) . sortBy (comparing fst)
  where
  f :: [(Elf, Maybe Score)] -> (Elf, Score)
  f bucket
    = let
        (elves, scores) = unzip bucket
      in (head elves, sumScore scores)
    


playGame :: Score -> [Elf] -> [(Elf, Score)]
playGame maxMarbleScore players
  = let
      turns = cycle players
      marbles = map Marble [1 .. maxMarbleScore]
      initialGame = fromList [Marble 0]
      (_, moveScores) = foldl foldPlay (initialGame, []) marbles
      playerScores = zip turns moveScores
    in collateScores playerScores

main :: IO()
main = do
  let scores = playGame 7090400 (map Elf [1..473])
  putStrLn . show $ scores
  putStrLn . show . maximumBy (comparing snd) $ scores
