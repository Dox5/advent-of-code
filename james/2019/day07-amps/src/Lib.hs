module Lib where

import qualified IntCode as IntCode

import qualified Data.List as List

program :: IntCode.Program
program = IntCode.load [3,8,1001,8,10,8,105,1,0,0,21,38,55,72,93,118,199,280,361,442,99999,3,9,1001,9,2,9,1002,9,5,9,101,4,9,9,4,9,99,3,9,1002,9,3,9,1001,9,5,9,1002,9,4,9,4,9,99,3,9,101,4,9,9,1002,9,3,9,1001,9,4,9,4,9,99,3,9,1002,9,4,9,1001,9,4,9,102,5,9,9,1001,9,4,9,4,9,99,3,9,101,3,9,9,1002,9,3,9,1001,9,3,9,102,5,9,9,101,4,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,99,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99]

phaseSettings :: [[Int]]
phaseSettings = List.permutations [0, 1, 2, 3, 4]

phaseSettingsFeedback :: [[Int]]
phaseSettingsFeedback = List.permutations [5, 6, 7, 8, 9]

runAmps :: Int -> [Int] -> Int
runAmps thrustPower [] = thrustPower
runAmps ipt (phase:phases) =
  let
    (ampOutput:[]) = IntCode.output . IntCode.runWithInput [phase, ipt] $ program
  in runAmps ampOutput phases

bestPower :: Int
bestPower = List.foldl1' max $ fmap (runAmps 0) phaseSettings

type Amp = (IntCode.Suspended, IntCode.VMState)

feedbackAmps :: [Int] -> [Amp]
feedbackAmps phases = fmap build phases
  where
    build :: Int -> Amp
    build p =
      let
        amp@(suspend, _) = IntCode.startVM program
      in case suspend of
        IntCode.InputRequested -> IntCode.resumeVMWithInput p amp
        _ -> error "Error on creation, expected to provide input!"

ampScheduler :: Int -> [Amp] -> ([Amp] -> [Amp]) -> (Int, [Amp])
ampScheduler ipt [] amps = (ipt, amps [])
ampScheduler ipt (a:amps) prevAmps =
  let
    a'@(suspend, _) = IntCode.resumeVMWithInput ipt a
  in case suspend of
    (IntCode.Output v) -> ampScheduler v amps (prevAmps . (a':))
    (IntCode.Halted)   -> ampScheduler 0 amps id
    (IntCode.InputRequested) -> error "Should not be possible with execution model"

runUntilHalted :: Int -> [Amp] -> [Int]
runUntilHalted ipt [] = ipt : []
runUntilHalted ipt amps =
  let
    (output, newAmps) = ampScheduler ipt amps id
  in output : (runUntilHalted output newAmps)

maxFeedback :: [Int] -> Int
maxFeedback phases =
  let
    amps = feedbackAmps phases
  in List.foldl1' max $ runUntilHalted 0 amps
