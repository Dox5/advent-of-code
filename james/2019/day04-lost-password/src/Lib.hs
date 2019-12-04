module Lib (
  potentialPassword,
  potentialPassword2,
  explode
)
  where

import qualified Data.List as List

-- Explode a number into a sequence of digits
explode :: Int -> [Int]
explode n = explode' n []
  where
    explode' :: Int -> [Int] -> [Int]
    explode' 0 digits = digits
    explode' x digits =
      let
        digit = x `rem` 10 
        x' = x `div` 10
      in explode' x' (digit:digits)

increasing :: [Int] -> Bool
increasing [] = True
increasing (_:[]) = True
increasing (x:y:rest) =
  if x > y then
    False
  else increasing (y:rest)

lengthIsTwo :: [a] -> Bool
lengthIsTwo (_:_:[]) = True
lengthIsTwo _ = False

atLeastOneDouble :: [Int] -> Bool
atLeastOneDouble = any lengthIsTwo . List.group

potentialPassword :: Int -> Bool
potentialPassword p = recurse (explode p) False
  where
    recurse :: [Int] -> Bool -> Bool
    recurse [] seenDouble = seenDouble
    recurse (_:[]) seenDouble = seenDouble
    recurse (x:y:rest) seenDouble =
      if x == y then
        recurse (y:rest) True
      else if x < y then
        recurse (y:rest) seenDouble
      else
        False

potentialPassword2 :: Int -> Bool
potentialPassword2  p = 
  let
    digits = explode p
  in increasing digits && atLeastOneDouble digits

