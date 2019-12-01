module Main(main) where

import qualified Data.List as List
import qualified Data.Tuple as Tuple
import qualified GHC.Exts as Exts
import qualified System.IO as IO

--instance Read Claim where
--  readsPrec _ s = [(Clm id $ Bx point size, remaining) | ("#", t)   <- lex s,
--                                                        (id, u)    <- reads t,
--                                                        ("@", v)   <- lex u,
--                                                        (point, w) <- reads v,
--                                                        (":", x)   <- lex w,
--                                                        (size, remaining) <- reads x]

data What = WakeUp | FallAsleep | BeginShift Int deriving (Show, Eq)

readWhat :: String -> [(What, String)]
readWhat line = uncurry readWhat' $ head . lex $ line
  where
    readWhat' :: String -> String -> [(What, String)]
    readWhat' "Guard" str = [(BeginShift id, remaining)
      | ("#",      r1) <- lex str,
        (id,       r2) <- reads r1,
        ("begins", r3) <- lex r2,
        ("shift",  remaining) <- lex r3]

    readWhat' "wakes" str = [(WakeUp, remaining) 
      | ("up", remaining) <- lex str]

    readWhat' "falls" str = [(FallAsleep, remaining)
      | ("asleep", remaining) <- lex str]

instance Read What where
  readsPrec _ s = readWhat s

type Year   = Int
type Month  = Int
type Day    = Int
type Hour   = Int
type Minute = Int

data When = When Year Month Day Hour Minute deriving (Show, Ord, Eq)

instance Read When where
  readsPrec _ s = [((When year month day hour minute), remaining)
    | ("[",   r1)  <- lex s,
      (year,  r2)  <- reads r1,
      ("-",   r3)  <- lex r2,
      (month, r4)  <- reads r3,
      ("-",   r5)  <- lex r4,
      (day,   r6)  <- reads r5,
      (hour,  r7)  <- reads r6,
      (":",   r8)  <- lex r7,
      (minute, r9) <- reads r8,
      ("]", remaining) <- lex r9]

data Event = Event What When deriving (Show, Eq)

instance Read Event where
  readsPrec _ s = [(Event what when, remaining) | (when, r1) <- reads s,
                                     (what, remaining) <- reads r1]

instance Ord Event where
  (<=) (Event _ lwhen) (Event _ rwhen) = lwhen <= rwhen 

grabMinute :: When -> Minute
grabMinute (When _ _ _ _ minute) = minute

-- When : time at which they fell asleep
-- Int  : How many (full) minutes they slept for 
data Asleep = Asleep When Int deriving (Show)

-- Take a chronologically ordered list of events and group each non 'BeginShift'
-- event with the start of that shift
shiftActivities :: [Event] -> [(Event, [Event])]
shiftActivities events = activities events []
  where
    activities :: [Event] -> [(Event, [Event])] -> [(Event, [Event])]
    activities (shift:events) = uncurry makeShift $ span notShiftStart events
      where
        makeShift :: [Event] -> [Event] -> [(Event, [Event])] -> [(Event, [Event])]
        makeShift inShift nextShift = ((shift, inShift) :) . activities nextShift
    activities [] = id


    notShiftStart :: Event -> Bool
    notShiftStart (Event (BeginShift _) _) = False
    notShiftStart _                        = True

-- Take a list of related FallAsleep/WakeUp events and turns them into Asleep's
pairSleepWake :: [Event] -> [Asleep]
pairSleepWake x = pair x []
  where
    pair :: [Event] -> [Asleep] -> [Asleep]
    pair (fall:wake:events) = (makeAsleep fall wake :) .  pair events
    pair [] = id
    
    makeAsleep :: Event -> Event -> Asleep
    makeAsleep (Event (FallAsleep) startTime) (Event (WakeUp) endTime) = Asleep startTime period
      where
        period = (grabMinute endTime) - (grabMinute startTime)

type Guard = Int 

guardAndSnoozes :: (Event, [Event]) -> (Guard, [Asleep])
guardAndSnoozes ((Event (BeginShift guard) _), events) = (guard, pairSleepWake events)

combineDays :: [(Guard, [Asleep])] -> [(Guard, [Asleep])]
combineDays splitDays = map (\activity -> (theGuard activity, snoozes activity)) $ groupedByGuard
  where
    groupedByGuard = Exts.groupWith (\(guard, _) -> guard) splitDays

    theGuard :: [(Guard, [Asleep])] -> Guard
    theGuard = fst . head

    snoozes :: [(Guard, [Asleep])] -> [Asleep]
    snoozes guardActivity = concatMap (\(_, s) -> s) guardActivity

totalTimeAsleep :: [Asleep] -> Int
totalTimeAsleep snoozes = sum . map (\(Asleep _ time) -> time) $ snoozes
    
-- Returns all the times that the guard was asleep at as a single int
-- (minutes past 00:00)
minutesSpentAsleep :: Asleep -> [Int]
minutesSpentAsleep (Asleep (When _ _ _ _ startedAt) snoozeLength) = [startedAt..(startedAt + snoozeLength - 1)]

type TimeSlot = Int

bestTimeSlot :: [Asleep] -> (TimeSlot, Int)
bestTimeSlot snoozes = List.maximumBy (\l r -> compare (snd l) (snd r)) $ instancesAndTime
  where
    instancesAndTime = map w .  List.group . List.sort $ asleepAt
      where w times = (head times, length times)
    asleepAt = concatMap minutesSpentAsleep snoozes

bestTimeSlotForGuard :: (Guard, [Asleep]) -> (Guard, TimeSlot, Int)
bestTimeSlotForGuard (guard, snoozes) = (guard, ts, count)
  where
    ts = fst best
    count = snd best
    best = bestTimeSlot snoozes

maybeGoAt :: Maybe (Guard, [Asleep]) -> TimeSlot
maybeGoAt (Just x) = timeSlot . bestTimeSlotForGuard $ x
  where
    timeSlot (_, ts, _) = ts

mostConsistantSnoozer :: [(Guard, [Asleep])] -> (Guard, TimeSlot)
mostConsistantSnoozer guardActivity = onlyGuardAndTimeSlot thisGuy
  where
    frequency (_, _, l) (_, _, r) = compare l r
    guardsWhoSnooze = filter (\(_, snoozes) -> (length snoozes)  > 0) $ guardActivity
    thisGuy = List.maximumBy frequency . map bestTimeSlotForGuard $ guardsWhoSnooze
    onlyGuardAndTimeSlot (g, t, _) = (g, t)


getEventLog :: IO.FilePath -> IO [Event]
getEventLog path = do
  contents <- IO.readFile path
  return . map (fst . head . reads) . lines $ contents


main :: IO ()
main = do
  let input = "input.txt"
  events <- getEventLog input

  let guardSnoozes = combineDays . map guardAndSnoozes . shiftActivities . List.sort $ events 
  let sleepiestGuard = List.maximumBy (\l r -> compare (snd l) (snd r)) . map (\(guard, snoozes) -> (guard, totalTimeAsleep snoozes)) $ guardSnoozes
  let guardId = fst sleepiestGuard

  putStrLn . showString "Guard " . shows (fst sleepiestGuard) . showString " slept the longest with " . shows (snd sleepiestGuard) $ " minutes of snooze time"

  let goAt = maybeGoAt . List.find (\(guard, _) -> guard == guardId) $ guardSnoozes
  putStrLn . showString "Best time to go at is 00:" . shows goAt $ ""

  let consistant = mostConsistantSnoozer guardSnoozes
  putStrLn . showString "Alternatively guard " . shows (fst consistant) . showString " snoozes most consistantly at 00:" . shows (snd consistant) $ ""
