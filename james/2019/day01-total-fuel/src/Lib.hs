module Lib
    ( calcFuelRequirement,
      calcCompoundFuelRequirement
    ) where

-- When something weighs below this amount, just wish really hard for to have
-- enough fuel to cover it!
wishingWeight :: Int
wishingWeight = 6

calcFuelRequirement :: Int -> Int
calcFuelRequirement weight = divWeight - 2
  where
    divWeight = weight `div` 3

calcCompoundFuelRequirement :: Int -> Int
calcCompoundFuelRequirement module_weight = recurse 0 module_weight
  where
    recurse :: Int -> Int -> Int
    recurse accum weight = if weight <= wishingWeight then
                             accum
                           else 
                             let 
                               requiredFuel = calcFuelRequirement weight
                               compound = recurse (accum + requiredFuel) requiredFuel
                             in compound
