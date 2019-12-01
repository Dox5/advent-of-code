module Main(main) where


import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Matrix as Matrix
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified System.IO as IO

import Data.Ord(comparing)

newtype Instruction = Inst Char deriving(Show, Ord, Eq)

-- left instruction must be done before right instruction
data Dependency = DoneBefore Instruction Instruction deriving (Show)

instance Read Dependency where
  -- Cheating, we want character 5 and 36
  readsPrec _ s = [(DoneBefore (Inst from) (Inst to), "")]
    where
      from = (s !! 5)
      to   = (s !! 36)

data Worker = WorkingOn Int Int | Idle deriving (Show, Eq)

type Graph = Matrix.Matrix Int

type IdOf = (Instruction -> Int)

uniqueInstructions :: [Instruction] -> [Instruction]
uniqueInstructions = List.sort . Set.toList . Set.fromList

-- Make a list of every unique instruction in the instruction set
allInstructions :: [Dependency] -> [Instruction]
allInstructions deps = uniqueInstructions instructions
  where
    instructions = concatMap (\(DoneBefore a b) -> [a, b]) deps

instructionId :: [Dependency] -> Instruction -> Int
instructionId deps = idOf
  where
    steps = uniqueInstructions . allInstructions $ deps

    idOf :: Instruction -> Int
    idOf i = 1 + (Maybe.fromJust . List.elemIndex i $ steps)

idInstruction :: [Dependency] -> Int -> Instruction
idInstruction deps i = steps !! index
  where
    steps = uniqueInstructions . allInstructions $ deps
    index = i - 1

instTime :: [Dependency] -> Int -> Int
instTime deps i = time . letter . ofId $ i
  where
    ofId = idInstruction deps
    letter (Inst l) = l
    time = (-4 + ) . Char.ord

insertDependencies :: IdOf -> [Dependency] -> Graph -> Graph
insertDependencies idOf deps initial = foldr addToGraph initial deps
  where
    addToGraph :: Dependency -> Graph -> Graph
    addToGraph (DoneBefore from to) = Matrix.setElem 1 (row, col)
      where
        row = idOf to
        col = idOf from

sumIsZero :: [Int] -> Bool
sumIsZero nums = all (0==) nums

-- Next instruction is the lowest indexed row that sums to exactly 0
nextInstruction :: Graph -> Int
nextInstruction graph = 1 + rowIndex
  where
    rowIndex = Maybe.fromJust . List.elemIndex True $ candidateRows
    candidateRows = map sumIsZero . Matrix.toLists $ graph

walkByNoIncoming :: Graph -> [Int]
walkByNoIncoming graph
  | done graph = []
  | otherwise  = next:(walkByNoIncoming (markUsed next graph))
  where
    next = nextInstruction graph
    done =  ((-Matrix.ncols graph) ==) . Matrix.trace

-- Marks the instruction as 'used' (ie it's been included in the output already
-- This is done by marking the self-loop to -1
-- This also sets the column to 0
markUsed :: Int -> Graph -> Graph
markUsed i = Matrix.setElem (-1) (i,i) . setColToZero
  where
    setColToZero = Matrix.mapCol (\_ _ -> 0) i

graphDependencies :: [Dependency] -> Graph
graphDependencies deps = graph
  where
    idOf     = instructionId deps
    idToInst = idInstruction deps
    nSteps   = length . uniqueInstructions . allInstructions $ deps
    graph    = insertDependencies (idOf) deps (Matrix.zero nSteps nSteps)

assemblyOrder :: [Dependency] -> [Instruction]
assemblyOrder deps = map idToInst order
  where
    idToInst = idInstruction deps
    order    = walkByNoIncoming . graphDependencies $ deps

instructionString :: [Instruction] -> String
instructionString = map (\(Inst i) -> i)

getInstructionOrder :: IO.FilePath -> IO [Dependency]
getInstructionOrder path = do
  contents <- IO.readFile path
  return . map (fst . head . reads) . lines $ contents

nIdleWorkers :: Int -> [Worker]
nIdleWorkers n = [Idle | _ <- [1..n]]

isWorking :: Worker -> Bool
isWorking (WorkingOn _ _) = True
isWorking _ = False

finishWorkAt :: Worker -> Int
finishWorkAt (WorkingOn _ time) = time


markStarted :: Int -> Graph -> Graph
markStarted i = Matrix.setElem (-1) (i,i)

notStarted :: Int -> Graph -> Bool
notStarted i = (0==) . (Matrix.getElem i i)

availableWork :: Graph -> [Int]
availableWork work = map snd . filter ((True==) . fst) $ indexedRows
  where
    candidateRows = map sumIsZero . Matrix.toLists $ work
    indexedRows   = zip candidateRows [1..]
    

duration :: Worker -> Int
duration (WorkingOn _ d) = d
    
nextActivityIn :: [Worker] -> Int
nextActivityIn active = duration nextToFinish
  where
    nextToFinish = List.minimumBy (comparing finishWorkAt) . filter isWorking $ active

updateDuration :: Int -> [Worker] -> [Worker]
updateDuration d = map subtractD
  where
    subtractD :: Worker -> Worker
    subtractD (WorkingOn i remaining) = if remaining == d then Idle else (WorkingOn i (remaining - d))
    subtractD Idle = Idle

finishWorkByDuration :: Int -> [Worker] -> Graph -> Graph
finishWorkByDuration d workers work = foldr markUsed work finished
  where
    finished = map (\(WorkingOn i _) -> i) . filter (\x -> isWorking x && (duration x) == d) $ workers

-- Match work that can be started with workers that are slacking
getBackToWork :: (Int -> Int) -> [Worker] -> Graph -> ([Worker], Graph)
getBackToWork instTime workers work = (concat [allWorking, padding], startedGraph)
  where
    numIdle = length . filter (not . isWorking) $ workers
    newWork = take numIdle (availableWork work)
    taskedWorkers = map (\i -> WorkingOn i (instTime i)) newWork
    allWorking    = concat([taskedWorkers, filter isWorking workers])
    padding = [(Idle) | _ <- [start..end]]
      where
        start = length allWorking
        end   = (length workers) - 1
    startedGraph = foldr markStarted work newWork

x :: (Int -> Int) -> ([Worker], Graph) -> Int -> Int
x instTime (workers, work) timeTaken 
  | all (not . isWorking) workers = timeTaken
  | otherwise = (x instTime initiatedWork (timeTaken + nextWorkerFreeIn))
    where
      nextWorkerFreeIn = nextActivityIn workers
      updatedWorkers = updateDuration nextWorkerFreeIn workers
      initiatedWork = getBackToWork instTime updatedWorkers (finishWorkByDuration nextWorkerFreeIn workers work)

timeTakenToBuild :: Int -> [Dependency] -> Int
timeTakenToBuild nWorkers deps = x iTime startWork 0
  where
    iTime     = instTime deps
    idOf      = instructionId deps
    nSteps    = length . uniqueInstructions . allInstructions $ deps
    work      = insertDependencies (idOf) deps (Matrix.zero nSteps nSteps)
    workers   = nIdleWorkers nWorkers
    startWork = getBackToWork iTime workers work

main :: IO ()
main = do
  let input = "input.txt"
  ins <- getInstructionOrder input
  let order = assemblyOrder ins
  putStrLn . showString (instructionString order) $ ""
  let nWorkers = 5
  putStrLn . showString "Will take " . shows (timeTakenToBuild nWorkers ins) . showString " seconds to build with " . shows nWorkers . showString " workers" $ ""
