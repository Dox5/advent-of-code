{-# LANGUAGE TemplateHaskell #-}
module TestElfScore(runTests) where

import ElfScore
import Tape

import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Maybe (isNothing)

import Test.QuickCheck.All

import Debug.Trace (trace)

winningScore :: Int -> Score -> Score
winningScore players lastMarble
  = let
      playerScores = playGame lastMarble (map Elf [1..players])
    in snd . maximumBy (comparing snd) $ playerScores

prop_1618 = winningScore 10 1618 == 8317
prop_7999 = winningScore 13 7999 == 146373
prop_1104 = winningScore 17 1104 == 2764
prop_6111 = winningScore 21 6111 == 54718
prop_5807 = winningScore 30 5807 == 37305

prop_normal_move
  = let
    game = fromList . map Marble $ [20,10,5,11,1,12,6,13,3,14,7,15,0,16,8,17,4,18,9,19,2]
    peice = Marble 21
    expectedPrefix = map Marble [10, 20]
    expectedSuffix = map Marble [5,11,1,12,6,13,3,14,7,15,0,16,8,17,4,18,9,19,2]
    expectedGame = Tape expectedPrefix (Marble 21) expectedSuffix
    (actualGame, score) = playMarble game peice
  in actualGame == expectedGame && isNothing score

prop_scoring_move
  = let
    game = fromList . map Marble $ [22, 11, 1, 12, 6, 13, 3, 14, 7, 15, 0, 16, 8, 17, 4, 18, 9, 19, 2, 20, 10, 21, 5]
    peice = Marble 23
    (actualGame, score) = playMarble game peice
    expectedGame = Tape (map Marble [18, 4, 17, 8, 16, 0, 15, 7, 14, 3, 13, 6, 12, 1, 11, 22]) (Marble 19) (map Marble [2, 20, 10, 21, 5])
  in trace (show actualGame ++ " == " ++ show expectedGame) actualGame == expectedGame

return []

runTests :: IO Bool
runTests = $quickCheckAll
