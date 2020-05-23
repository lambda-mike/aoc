module Day04.Day04 where

import Data.List as L
import qualified Data.Map.Strict as Map

type Id = Int
type Minute = Int
type SleepLog = Map.Map Id (Map.Map Minute Int)

data EventType
  = Start Id
  | Sleep Minute
  | WakeUp Minute
  deriving (Show, Eq)

data Event =
  Event
    { date :: String
    , event :: EventType
    }
    deriving (Show, Eq)

data State =
  State
    { id' :: Id
    , lastEvent :: Maybe Event
    , sleepLog :: SleepLog
    }

solveA :: String -> Int
solveA =
  calculateResult
  . findIdAndMinute
  . generateSleepLog
  . sortChronologically
  . parseEvents
  . lines

solveB :: String -> Int
solveB =
  calculateResult
  . findIdAndMostPopularMinute
  . generateSleepLog
  . sortChronologically
  . parseEvents
  . lines

parseEvents :: [String] -> [Event]
parseEvents = map parseEvent

sortChronologically :: [Event] -> [Event]
sortChronologically =
  sortBy compareEvents
  where
    compareEvents e1 e2 =
      compare (date e1) (date e2)

generateSleepLog :: [Event] -> SleepLog
generateSleepLog events =
  let
    initialState =
      State { id' = -1
            , lastEvent = Nothing
            , sleepLog = Map.empty
            }
    finalState = foldl mergeEvent initialState events
  in
    sleepLog finalState

mergeEvent :: State -> Event -> State
mergeEvent state event@Event { date = date, event = eventType } =
  case eventType of
    Start id' ->
      state { id' = id'
            , lastEvent = Just event
            ,  sleepLog = addId (sleepLog state) id'
            }
    Sleep m ->
      state { lastEvent = Just event }
    WakeUp m ->
      state { lastEvent = Just event
            , sleepLog = addSleep state m
            }

addId :: SleepLog -> Id -> SleepLog
addId sleepLog id' =
  Map.insertWith ignoreNewValue id' Map.empty sleepLog
  where
    ignoreNewValue = flip const

addSleep :: State -> Minute -> SleepLog
addSleep state wakeUpMinute =
  let
    sleepLog' = sleepLog state
    id'' = id' state
    minutesLog = (Map.!) sleepLog' id''
    (Just (Sleep sleepMinute)) = event <$> lastEvent state
    newMinutesLog =
      L.foldr (\(k, v) ml ->
        Map.insertWith (+) k v ml) minutesLog .
      L.map (\m -> (m, 1)) $
      [sleepMinute..wakeUpMinute-1]
  in
    Map.insert id'' newMinutesLog sleepLog'

findIdAndMinute :: SleepLog -> (Id, Minute)
findIdAndMinute sleepLog =
  let
    (id'', minutes)
      = L.head
      . Map.toList
      . Map.foldrWithKey longestSleep Map.empty
      $ sleepLog
    (minute, times) =
      findMostPopularMinute minutes
  in
    (id'', minute)

findIdAndMostPopularMinute :: SleepLog -> (Id, Minute)
findIdAndMostPopularMinute sleepLog =
  let
    (id'', minutes)
      = L.head
      . Map.toList
      . Map.foldrWithKey mostFrequentSleep Map.empty
      $ sleepLog
    (minute, times) =
      findMostPopularMinute minutes
  in
    (id'', minute)

mostFrequentSleep
  :: Id
  -> Map.Map Minute Int
  -> SleepLog
  -> SleepLog
mostFrequentSleep id'' minutes maxSoFar
  | Map.empty == maxSoFar =
    createSleepLog id'' $ createMinutesDict ( findMostPopularMinute minutes )
  | otherwise =
    let
      maxSoFarFrequentMinuteCount =
        getDictValue .
        getDictValue $
        maxSoFar
      newFrequentMinuteAndCount@(newFrequentMinute, newFrequentMinuteCount) =
        findMostPopularMinute minutes
    in
      if newFrequentMinuteCount > maxSoFarFrequentMinuteCount then
        createSleepLog id'' $ createMinutesDict newFrequentMinuteAndCount
      else
        maxSoFar
  where
    createSleepLog id'' minutes =
      Map.fromList [ (id'', minutes ) ]
    createMinutesDict minAndCount =
      Map.fromList [ minAndCount ]
    getDictValue =
      snd . head .Map.toList

findMostPopularMinute :: Map.Map Minute Int -> (Minute, Int)
findMostPopularMinute =
  Map.foldrWithKey
    (\k v (m, mv) ->
      if v > mv
      then (k, v)
      else (m, mv)
    )
    (-1, -1)

longestSleep :: Id -> Map.Map Minute Int -> SleepLog -> SleepLog
longestSleep id'' minutes maxSoFar
  | Map.empty == maxSoFar =
    Map.fromList [(id'', minutes)]
  | otherwise =
    let
      newTotalSleep = calculateTotalSleep minutes
      maxSoFarTotalSleep =
          calculateTotalSleep .
          snd .
          L.head .
          Map.toList
          $ maxSoFar
    in
      if newTotalSleep > maxSoFarTotalSleep then
          Map.fromList [(id'', minutes)]
      else
          maxSoFar

calculateTotalSleep :: Map.Map Minute Int -> Int
calculateTotalSleep =
  Map.foldr (+) 0

calculateResult :: (Id, Minute) -> Int
calculateResult (id', min') = id' * min'

parseEvent :: String -> Event
parseEvent str =
  let
    chunks = words str
    date = unwords $ L.take 2 chunks
    eventChunks = L.drop 2 chunks
    event = parseEventType date eventChunks
  in
    Event { date = date, event = event }

parseEventType
  :: String
  -> [String]
  -> EventType
parseEventType _    [ "Guard", idStr, _, _ ] = Start  . readInt . tail $ idStr
parseEventType date [ "falls", _ ]           = Sleep  $ parseMinutes date
parseEventType date [ "wakes", _ ]           = WakeUp $ parseMinutes date

parseMinutes :: String -> Int
parseMinutes =
  readInt . L.drop 3 . L.init . L.head . L.tail . words

readInt :: String -> Int
readInt = read

startEvt = "[1518-11-01 00:00] Guard #10 begins shift"
sleepEvt = "[1518-11-01 00:05] falls asleep"
wakeUpEvt = "[1518-11-01 00:25] wakes up"

sample = "[1518-11-01 00:00] Guard #10 begins shift\n[1518-11-01 00:05] falls asleep\n[1518-11-01 00:25] wakes up\n[1518-11-01 00:30] falls asleep\n[1518-11-01 00:55] wakes up\n[1518-11-01 23:58] Guard #99 begins shift\n[1518-11-02 00:40] falls asleep\n[1518-11-02 00:50] wakes up\n[1518-11-03 00:05] Guard #10 begins shift\n[1518-11-03 00:24] falls asleep\n[1518-11-03 00:29] wakes up\n[1518-11-04 00:02] Guard #99 begins shift\n[1518-11-04 00:36] falls asleep\n[1518-11-04 00:46] wakes up\n[1518-11-05 00:03] Guard #99 begins shift\n[1518-11-05 00:45] falls asleep\n[1518-11-05 00:55] wakes up"

