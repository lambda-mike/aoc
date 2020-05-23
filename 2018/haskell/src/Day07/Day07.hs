module Day07.Day07 where

import Data.Char (ord)
import qualified Data.List as L
import qualified Data.Set as Set
import qualified Data.Map as Map


type Step = Char
type Dependencies = Map.Map Step (Set.Set Step)
type TimeLeft = Int
type Worker = (Step, TimeLeft)


solveA :: String -> String
solveA =
  orderSteps
  . createStepsMap
  . parseDependencies
  . lines

solveB :: String -> Int
solveB =
  calculateTotalTime
  . createStepsMap
  . parseDependencies
  . lines

calculateTotalTime :: Dependencies -> Int
calculateTotalTime dependencies =
  executeStepsB dependencies nextSteps workers total
  where
    nextSteps   = Set.empty
    workers     = []
    total       = 0

orderSteps :: Dependencies -> String
orderSteps dependencies =
  reverse
    $ executeSteps dependencies Set.empty ""

executeSteps :: Dependencies -> Set.Set Step -> String -> String
executeSteps dependencies nextSteps instruction
  | Map.null dependencies = addFinalSteps instruction nextSteps
  | otherwise             =
    let
      (noDeps, deps) =
        Map.partition Set.null dependencies
      noDepsSet =
        Map.keysSet noDeps
      (step, newNextSteps) =
        Set.deleteFindMin $ Set.union nextSteps noDepsSet
      newDeps =
        Map.map (flip Set.difference $ Set.singleton step) deps
    in
      executeSteps newDeps newNextSteps (step : instruction)

executeStepsB :: Dependencies -> Set.Set Step -> [Worker] -> Int -> Int
executeStepsB dependencies nextSteps workers total
    | executionFinished dependencies nextSteps workers = total
    | otherwise =
        -- 1. fill next steps, clean dependencies (remove deps with no prerequisities)
        -- 2. redistribute work among workers
        -- 3. work - find the lowest, decrease all workers, replace lowest worker with nothing, increase total, remove finished step from dependencies
        let
          (noDeps, deps) =
            Map.partition Set.null dependencies
          noDepsSet =
            Map.keysSet noDeps
          nextStepsBeforeWork =
            Set.union nextSteps noDepsSet
          (newWorkers, nextStepsAfterWork) =
            redistributeWork workers nextStepsBeforeWork
          lowestTimeLeft = findLowestTimeLeft newWorkers
          (workersAfterWork, workersDone) = work newWorkers lowestTimeLeft
          newTotal = total + lowestTimeLeft
          newDeps = removeSteps deps workersDone
        in
          executeStepsB newDeps nextStepsAfterWork workersAfterWork newTotal

removeSteps :: Dependencies -> [Worker] -> Dependencies
removeSteps deps workers =
  let doneSteps =
        Set.fromList $ map fst workers
  in  Map.map (flip Set.difference doneSteps) deps

work :: [Worker] -> TimeLeft -> ([Worker], [Worker])
work workers timeLeft =
  L.partition
    ((/=) 0 . snd)
  $ map
      (decrease timeLeft)
      workers
  where
    decrease dt (s, t) = (s, t - dt)

findLowestTimeLeft :: [Worker] -> TimeLeft
findLowestTimeLeft =
  minimum . map snd

redistributeWork
  :: [Worker]
  -> Set.Set Step
  -> ([Worker], Set.Set Step)
redistributeWork workers steps =
  let workingNum                    = length workers
      (stepsToTake, remainingSteps) = splitSteps workingNum steps
      newWorkers                    = map initWorker $ Set.elems stepsToTake
  in
    (workers ++ newWorkers, remainingSteps)

initWorker :: Step -> Worker
initWorker step =
    (step, calculateTimeForStep step)

calculateTimeForStep :: Step -> TimeLeft
calculateTimeForStep step =
  let taskOverhead = 60
      aLetterCode  = Data.Char.ord 'A'
      stepCode     = Data.Char.ord step
  in  taskOverhead + (stepCode - aLetterCode + 1)

splitSteps :: Int -> Set.Set Step -> (Set.Set Step, Set.Set Step)
splitSteps workingNum steps =
  let idleNum =
        workersNum - workingNum
  in Set.splitAt idleNum steps

executionFinished :: Dependencies -> Set.Set Step -> [Worker] -> Bool
executionFinished dependencies nextSteps workers =
  Map.null dependencies && Set.null nextSteps && workers == []

addFinalSteps :: String -> Set.Set Step -> String
addFinalSteps instruction nextSteps =
  foldr (:) instruction (Set.elems nextSteps)

createStepsMap :: [(Step, Step)] -> Dependencies
createStepsMap =
  Map.fromListWith Set.union .
  map (\(t, p) -> (t, createSet p))
  where
    createSet p
      | p == blank = Set.empty
      | otherwise  = Set.singleton p

parseDependencies :: [String] -> [(Step, Step)]
parseDependencies =
  concat . (map parseDependency)

parseDependency :: String -> [(Step, Step)]
parseDependency input =
  let
    chunks = words input
    prerequisite = getStep $ tail chunks
    target = getStep $ drop 7 chunks
  in
    [ (target, prerequisite)
    , (prerequisite, blank)
    ]
  where
    getStep = head . head

blank :: Char
blank = ' '

workersNum :: Int
workersNum = 5

sample = "Step C must be finished before step A can begin.\nStep C must be finished before step F can begin.\nStep A must be finished before step B can begin.\nStep A must be finished before step D can begin.\nStep B must be finished before step E can begin.\nStep D must be finished before step E can begin.\nStep F must be finished before step E can begin."

line = "Step C must be finished before step A can begin."
