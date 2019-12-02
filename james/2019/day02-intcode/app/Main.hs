module Main where

import qualified Vm as Vm

initialMemory :: Int -> Int -> Vm.Program
initialMemory noun verb = Vm.load
  [   1,noun,verb,   3,   1,   1,   2,   3,   1,   3,   4,   3,   1,   5,   0,
      3,   2,   1,  10,  19,   2,   9,  19,  23,   2,  23,  10,  27,   1,   6, 
     27,  31,   1,  31,   6,  35,   2,  35,  10,  39,   1,  39,   5,  43,   2,
      6,  43,  47,   2,  47,  10,  51,   1,  51,   6,  55,   1,  55,   6,  59,
      1,   9,  59,  63,   1,  63,   9,  67,   1,  67,   6,  71,   2,  71,  13,
     75,   1,  75,   5,  79,   1,  79,   9,  83,   2,   6,  83,  87,   1,  87,
      5,  91,   2,   6,  91,  95,   1,  95,   9,  99,   2,   6,  99, 103,   1,
      5, 103, 107,   1,   6, 107, 111,   1, 111,  10, 115,   2, 115,  13, 119,
      1, 119,   6, 123,   1, 123,   2, 127,   1, 127,   5,   0,  99,   2,  14,
      0,   0]

part1Program :: Vm.Program
part1Program = initialMemory 12 2 

simpleResult :: Vm.Program -> Int
simpleResult = head . Vm.execute

part1 :: Int
part1 = simpleResult part1Program

searchSpace :: [(Int, Int)]
searchSpace = [(noun, verb) | noun <- [0..99], verb <- [0..99]]

-- Brute force part2
findSolution :: Int -> Maybe (Int, Int)
findSolution want = recurse searchSpace
  where
    recurse :: [(Int, Int)] -> Maybe (Int, Int)
    recurse [] = Nothing
    recurse ((noun, verb):toTry) = 
      let
        result = simpleResult . initialMemory noun $ verb
      in if result == want then Just (noun, verb) else recurse toTry

calcAns :: Maybe (Int, Int) -> Maybe Int
calcAns (Just (noun, verb)) = Just $ 100 * noun + verb
calcAns Nothing = Nothing

showPart2 :: Maybe Int -> ShowS
showPart2 (Just v) = shows v
showPart2 Nothing = showString "No solution found"

part2 :: Maybe Int
part2 =
  let
    maybeSolution = findSolution 19690720
  in calcAns maybeSolution

main :: IO()
main = do
  putStrLn . showString "Part1: " . shows part1 $ ""
  putStrLn . showString "Part2: " . showPart2 part2 $ ""
