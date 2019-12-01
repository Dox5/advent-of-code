{-# LANGUAGE TemplateHaskell #-}
module TestTape(runTests) where

import Tape

import Test.QuickCheck.All (quickCheckAll)

import Test.QuickCheck

instance Arbitrary a => Arbitrary (Tape a) where
  arbitrary
    = do
      max <- (1+) `fmap` getSize
      n   <- choose (0, max-1)
      (setOffset n . fromList) `fmap` (vector max)


prop_fromListLengthIsSame
  = let
    test l = (length . fromList $ l) == length l
  in forAll (listOf1 $ choose ('a', 'Z')) test

prop_offsetDoesNotChangeLength (NonEmpty l)
  = let
    test i = (length . setOffset i . fromList $ l) == (length l)
  in forAll (choose (0, (length l) - 1)) test

prop_offsetAndForwardGiveSameTape (NonEmpty l)
  = let
    tape = fromList l
    test offset = (setOffset offset tape) == (forward (offset) tape)
  in forAll (choose (0, (length l) - 1)) test

prop_forwardAndBackwardAreOpposites tape (Positive n)
  = let
    fwdbkwd = backward n . forward n $ tape
  in fwdbkwd == tape

prop_deleteDecreasesLength tape
  = length tape > 1 ==> length tape == (length . delete $ tape) + 1

prop_deleteReducesNumberOfElement tape
  = let
    elem = current tape
    countIn = length .  filter (elem==) . Tape.toList
    nBefore = countIn tape
    nAfter  = countIn . delete $ tape 
  in length tape > 1 ==> nBefore == nAfter+1

prop_currentUnchangedByInsert tape elem
  = let
      updated = insert elem tape  
  in (current tape) == (current updated)

prop_tapeLengthIncreasesByOneAfterInsert elem tape
  = let
    updated = insert elem tape
  in (length tape + 1) == (length updated)

prop_insertStepDeleteLeavesUnchanged tape elem
  = let
    updated = backward 1 . delete . forward 1 . insert elem $ tape
  in tape == updated

return []

runTests :: IO Bool
runTests = $quickCheckAll
