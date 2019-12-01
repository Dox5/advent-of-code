import qualified Data.Maybe as Maybe
import qualified Data.List as List

import Control.Monad (liftM)

import Data.Function (on)
import Data.Ord (comparing)

import Debug.Trace

import Test.QuickCheck ((==>), Arbitrary, Gen, arbitrary, choose, forAll, scale, shuffle, suchThat, sized)

newtype Marble = Marble Int deriving (Show, Eq, Ord)

newtype Elf = Elf Int deriving (Show, Eq, Ord)

newtype Score = Score Int deriving (Show, Eq, Ord)

data Game = GameNode {prev :: Game, marb :: Marble, next :: Game}

instance Arbitrary Game where
  arbitrary = sized arbitrary'
    where
      arbitrary' 0 = arbitrary' 1
      arbitrary' n =
        let
          bound = choose (1, n)
          marbles = fmap (map Marble) $ fmap (flip take [1..]) bound >>= shuffle
        in fromList `fmap` marbles

fromList :: [Marble] -> Game
fromList [] = error "Cannot build from empty list"
fromList marbles = let (first,last) = build last marbles first
                   in first

  where
    build :: Game -> [Marble] -> Game -> (Game, Game)
    build prev [] next = (next, prev)
    build prev (m:ms) next = let this = GameNode prev m rest
                                 (rest, last) = build this ms next
                             in (this, last)

toListForward :: Game -> [Marble]
toListForward (GameNode _ start first) = start : f first []
  where
    f :: Game -> [Marble] -> [Marble]
    f (GameNode _ marble n) = if marble /= start then ((marble :) . f n) else id

toListBackward :: Game -> [Marble]
toListBackward current = f (prev current) []
  where
    f :: Game -> [Marble] -> [Marble]
    f (GameNode p marble _) = if marble /= (marb current) then ((marble :) . f p) else (marble :)

sameForwardAndBackward g = (toListForward g) == (reverse . toListBackward $ g)

prop_gameForwardAndBackSame g = sameForwardAndBackward

takeForward :: Int -> Game -> [Marble]
takeForward 0 _ = []
takeForward n (GameNode _ m next) = m : takeForward (n-1) next

takeBackward :: Int -> Game -> [Marble]
takeBackward 0 _ = []
takeBackward n (GameNode prev m _) = m : takeBackward (n-1) prev

showsGame :: Game -> ShowS
showsGame (GameNode _ firstMarble next) = shows firstMarble . showsGame' firstMarble next
  where
    showsGame' :: Marble -> Game -> ShowS
    showsGame' stopAt (GameNode _ m  next) =
      if m == stopAt then id
      else (", " ++) . shows m . showsGame' firstMarble next


showGame :: Game -> String
showGame g = showsGame g ""

instance Show Game where
  showsPrec _ = showsGame

-- This is pretty confusing! Recusive loops are fun. Main thing to remember:
-- The _return_ values of the function are supplied as _arguments_ to that
-- function (but reversed in order). This is possible due to lazy evaluation
-- which is pretty cool. This causes the first and last elements to be passed to
-- eachother.
insertFront :: Game -> Marble -> Game
insertFront (GameNode _ firstMarble into) marble =
  let 
    this = GameNode last marble first' -- The new node we are inserting
    first' = GameNode this firstMarble first -- The new 'first' node (which is now second, naturally)
    (first, last) =  build first' into this -- Iterate the rest of the loop and build a new loop
  in this
  where
    build :: Game -> Game -> Game -> (Game, Game)
    build prev (GameNode _ m oldNext) next =
      let this = GameNode prev m rest
          (rest, last) = build this oldNext next
      in if m == firstMarble then
           (next, prev)
         else
           (this, last)

prop_insertedElementGameSameForwardsAndBackwards g
  = let
      marble = Marble 2048
      gameWithMarb = insertFront g marble
    in sameForwardAndBackward gameWithMarb

prop_insertedElementShouldBeFirst g
  = let
      marble = Marble 2048
      gameWithMarb = insertFront g marble
    in marble == (head . toListForward $ gameWithMarb)

deleteFront :: Game -> Game
deleteFront (GameNode _ firstMarble from) =
  let
    (first, last) = build last (from) first
  in
    first
  where
    build :: Game -> Game -> Game -> (Game, Game)
    build prev (GameNode _ m oldNext) next =
      let this = GameNode prev m rest
          (rest, last) = build this oldNext next
      in if m == firstMarble then
        (next, prev)
      else
        (this, last)

prop_gameFrontDeletedSameForwardsAndBackwards g
  = let
      pred = (length . toListForward $ g) > 1
    in pred ==> sameForwardAndBackward . deleteFront $ g

prop_gameAfterDeleteShouldBeSecondMarble g
  = let
      pred = (length . toListForward $ g) > 1
      sndMarble = marb . next $ g
      firstMarbleAfterDelete = marb . deleteFront $ g
    in pred ==> firstMarbleAfterDelete == sndMarble
    

walkForwards :: Int -> Game -> Game
walkForwards 0 game = game
walkForwards n (GameNode _ _ next) = walkForwards (n-1) next

prop_gameWalkedForwardsFirstMarble g
  = forAll (choose (0, 1000)) $ \n -> let
      walked = walkForwards n g
      expected = if n == 0 then marb g else head . drop n . cycle . toListForward $ g
    in (marb walked) == expected

walkBackwards :: Int -> Game -> Game
walkBackwards 0 game = game
walkBackwards n (GameNode prev _ _) = walkBackwards (n-1) prev

prop_gameWalkedBackwardsFirstMarble g
  = forAll (choose (0, 1000)) $ \n -> let
      walked = walkBackwards n g
      expected = if n == 0 then marb g else head . drop (n-1) . cycle . toListBackward $ g
    in (marb walked) == expected

everyN :: Int -> [a] -> [a]
everyN _ [] = []
everyN period lst = everyN' 1 lst
  where
    everyN' :: Int -> [a] -> [a]
    everyN' _ [] = []
    everyN' nth (x:xs) = if nth < period then (everyN' (nth+1) xs) else x : (everyN' 0 xs )

-- Using the game rules return an infinite list of the elves who score
scoringElfs :: Int -> [Elf]
scoringElfs nElves = map Elf . everyN 23 . cycle $ [1..nElves]

-- Needs to walk forwards 2 because we insert INFRONT
playPeice :: Game -> Marble -> Game
playPeice game marble = insertFront (walkForwards 2 game) marble

prop_peicePlaysInCorrectPlace g =
  let
    peice = Marble 2048
    asList = toListForward g
    expected = peice : (drop 2 asList) ++ (take 2 asList)
    actual = playPeice g peice
  in expected == (toListForward actual)

calcScore :: [Marble] -> Score
calcScore = Score . foldr (\(Marble v) -> (v+)) 0

playScoringPeice :: Marble -> Game -> (Game, Score)
playScoringPeice peice game =
  let
    extraPeice = walkBackwards 7 game
    updatedState = deleteFront $ extraPeice
  in
    (updatedState, calcScore [marb extraPeice, peice])

biggerGame :: Gen Game
biggerGame = scale (50+) arbitrary `suchThat` (\a -> (length . toListForward $ a) >= 14)

prop_playScoringPeice =
  forAll biggerGame  $ \g -> let
    peice = Marble 2048
    asList = toListBackward g
    extraScore = head . drop 6 . cycle $ asList
    expectedScore = calcScore [peice, extraScore]
    expectedGame  =  (drop 7 asList) ++ (take 6 asList)
    (actualGame, actualScore) = playScoringPeice peice g
  in (expectedGame == toListBackward actualGame) && (expectedScore == actualScore)

     

scoringMarble :: Marble -> Bool
scoringMarble (Marble v) = (v `mod` 23) == 0

makeMove :: Marble -> Game -> (Game, Maybe.Maybe Score)
makeMove m state 
  | scoringMarble m = let (updatedState, score) = playScoringPeice m state in (updatedState, Just score)
  | otherwise       = ((playPeice state m), Nothing)

value :: Marble -> Int
value (Marble v) = v


playGame :: Marble -> [Maybe Score]
playGame max
  = let
      initalState = (fromList [Marble 0], [])
      marbles = map Marble [1..(value max)]
      doMove (state, scores) peice
        = let
            (updated, score) = makeMove peice state
          in (updated, score:scores)
      (state, scores) = List.foldl' doMove initalState marbles
    in reverse scores

scoringMoves :: Marble -> [Score]
scoringMoves max = map Maybe.fromJust . filter Maybe.isJust . playGame $ max

sumScores :: [Maybe Score] -> Score
sumScores = foldl (\(Score v) (Score z) -> Score (v + z)) (Score 0) . map scoreOr0
  where
    scoreOr0 = Maybe.fromMaybe (Score 0)

winningScore :: Int -> Marble -> Score
winningScore nElves max = score . List.maximumBy (comparing score) $ elfPoints
  where
    elf (e, _) = e
    score (_, s) = s
    sumPoints :: [(Elf, Maybe Score)] -> (Elf, Score)
    sumPoints pts = let (elves, scores) = unzip pts in (head elves, sumScores scores)

    elves = cycle . map Elf $ [1..nElves]

    moves = playGame max
    elfPoints = map sumPoints .
                List.groupBy (on (==) elf) . 
                List.sortBy (comparing elf) .
                zip (elves) $ moves
    
main :: IO ()
main = do
  putStrLn . show . winningScore 473 $ (Marble 7090)
--  putStrLn . show . winningScore 473 $ (Marble 7090400)
