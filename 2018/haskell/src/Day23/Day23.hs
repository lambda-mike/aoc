module Day23.Day23 where

import Data.Function as F
import Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S

type X = Integer
type Y = Integer
type Z = Integer
type Distance = Integer

data Pos =
  Pos
    { x :: !X
    , y :: !Y
    , z :: !Z
    }
  deriving (Eq, Ord, Show)

data Robot =
  Robot
    { pos :: !Pos
    , r :: !Distance
    }
  deriving (Eq, Ord, Show)

data TraverseData =
  TraverseData
    { allRobots :: ![Robot]
    , currentSignal :: !Integer
    , maxSignal :: !Integer
    , robotsToCross :: ![Robot]
    -- current robot we are orbiting
    , baseRobot :: !Robot
    -- target tip we are heading to
    , targetPos :: !Pos
    , currentPoint :: !Pos
    -- iteration
    , i :: !Integer
    }
  deriving Show

pos0 :: Pos
pos0 = Pos 0 0 0

maxNeighbourhoodRange :: Integer
maxNeighbourhoodRange = 2


solveA
  :: String
  -> Int
solveA input =
  length
  $ findRobotsInRadius robots
  $ findRobotWithBiggestRadius robots
  where
    robots =
      parseNanobots input

solveB
  :: String
  -> Integer
solveB input =
  let robots =
        parseNanobots input
      overlappingRobots =
        generateOverlappingRobotsLookupTable robots
      overlappindRobotsNumberDict =
        countByNumberOfOverlappingRobotsInSet robots overlappingRobots
  in getStrongestPointClosestToStart
      robots
      overlappingRobots
      overlappindRobotsNumberDict

generateOverlappingRobotsLookupTable
  :: [Robot]
  -> Map Robot (Set Robot)
generateOverlappingRobotsLookupTable robots =
  M.fromList
  $ L.map (\r ->
      (r, S.fromList $ findOverlappingRobots robots r))
      robots

findOverlappingRobots
  :: [Robot]
  -> Robot
  -> [Robot]
findOverlappingRobots robots robot =
  L.filter
    (\bot ->
      distance (pos robot) (pos bot) <= r' + r bot)
    robots
  where
    r' = r robot

findRangeTipWithMaxSignal
  :: [Robot]
  -> [Robot]
  -> (Pos, Integer)
findRangeTipWithMaxSignal allRobots robotsToSearch =
  let tips =
        L.sortOn
          (\(p, _) -> distance pos0 p)
          maxTips
  in if tips == [] then
      error "could not find max tip closest to pos0"
     else
       head tips
  where
    maxTips =
      L.takeWhile
        (\(_, s) -> s == maxTipSignal)
        tipsWithSignals
    (_, maxTipSignal) =
      head tipsWithSignals
    tipsWithSignals =
      L.sortOn (\(_, s) -> (-1) * s)
      $ L.map (\p -> (p, calculateSignal allRobots p))
      $ L.concatMap
          generateRangeTips
          robotsToSearch

generateRangeTips
  :: Robot
  -> [Pos]
generateRangeTips Robot { pos = position, r = range } =
    [ position { x = x' - range }
    , position { x = x' + range }
    , position { y = y' - range }
    , position { y = y' + range }
    , position { z = z' - range }
    , position { z = z' + range }
    ]
  where
    x' = x position
    y' = y position
    z' = z position

countByNumberOfOverlappingRobotsInSet
  :: [Robot]
  -> Map Robot (Set Robot)
  -> Map Integer (Set (Set Robot))
countByNumberOfOverlappingRobotsInSet _robots overlappingRobots =
  M.foldlWithKey'
    (countOverlappingBotsClusters overlappingRobots)
    M.empty -- start with emtpy map
    overlappingRobots

countOverlappingBotsClusters
  :: Map Robot (Set Robot)
  -> Map Integer (Set (Set Robot))
  -> Robot
  -> Set Robot -- set of overlapping robots containing robot above
  -> Map Integer (Set (Set Robot))
countOverlappingBotsClusters
  robotsMap -- original map/dict
  robotsClusters -- result
  robot
  overlappingRobots =
    let intersectionOfAllOverlappingBots =
          S.foldl'
            (\setSoFar nanoBot ->
              let botsSet = robotsMap M.! nanoBot
              in botsSet `S.intersection` setSoFar
            )
            overlappingRobots
            overlappingRobots
    in M.alter (\setFamily ->
        case setFamily of
          Nothing ->
            Just $
              S.singleton
                intersectionOfAllOverlappingBots
          Just sf ->
            Just $
              S.insert
                intersectionOfAllOverlappingBots
                sf
        )
        (fromIntegral $ S.size intersectionOfAllOverlappingBots) -- key
        robotsClusters

getStrongestPointClosestToStart
  :: [Robot]
  -> Map Robot (Set Robot)
  -> Map Integer (Set (Set Robot))
  -> Integer
getStrongestPointClosestToStart
  robots
  overlappingRobots
  overlappindRobotsNumberDict =
  let (maxSignal', maxSet) = M.findMax overlappindRobotsNumberDict
      maxRobotSet =
        case S.size maxSet of
          0 -> error "no elements in set - fix your code"
          1 -> S.elemAt 0 maxSet
          _ -> error "implement traversal of all sets"
      -- assume always at least one robot
      maxRobots =
        S.toList maxRobotSet
      (startingPoint, startingSignal) =
        findRangeTipWithMaxSignal
          robots
          maxRobots
      startingRobot =
        let robots =
              L.filter
                (\rob -> distance (pos rob) startingPoint == r rob)
                maxRobots
        in if robots == [] then
             error "no maxRobots"
           else
             head robots
      otherRobots =
        L.filter
          (/= startingRobot)
          maxRobots
      robotsToCross' =
        L.filter
          (\rob ->
            distance (pos rob) startingPoint > r rob)
          otherRobots
      startingRobotTargetTip =
        -- get one of 4 tips which are neighbours to the max Signal tip
        -- get the one closest to pos0
        let tips =
              L.sortOn (distance pos0)
              $ L.take 4
              $ L.tail -- drop the startingPoint itself
              $ L.sortOn (distance startingPoint)
              $ generateRangeTips startingRobot
        in if tips == [] then
              error "Where are all my tips?!"
           else
              L.head tips
      traverseData =
        TraverseData
          { allRobots = robots
          , robotsToCross = robotsToCross'
          , maxSignal = maxSignal'
          , currentSignal = startingSignal
          , baseRobot = startingRobot
          -- In traversal it will be not yet visited robot?
          , targetPos = startingRobotTargetTip
          , currentPoint = startingPoint
          , i = 0
          }
      traversalResult =
        quasiSimplexTraverse traverseData
      closestPos =
        findPosClosestToPos0
          (allRobots traversalResult)
          (maxSignal traversalResult)
          (currentPoint traversalResult)
  in distance pos0 closestPos

-- Traverse edges of nanobots ranges to find the strongest range area
-- Assumption1: only robots from overlapping set above can be crossed
-- in our traversal
quasiSimplexTraverse
  :: TraverseData
  -> TraverseData
quasiSimplexTraverse td =
  if robotsToCross' == [] && currentSignal' /= maxSignal' then
    error "Check neighbours signals here maybe"
  else if currentSignal' == maxSignal' then
    td
  else
    let neighbours =
          generateNeighboursInRange
            maxNeighbourhoodRange
            currentPoint'
        nextPointCandidates =
          L.filter
            (\p ->
              distance (pos baseRobot') p == r baseRobot' -- keep going on edge
              && distance p targetPos' < currentPointTargetDist
              && calculateSignal allRobots' p >= currentSignal'
            )
            neighbours
        newTraverseData =
          if nextPointCandidates == [] then
            switchToNewTraversalDirection
              td
              currentPoint'
              neighbours
          else
            td
              { currentPoint
                  = head nextPointCandidates
              }
        nextPoint =
          currentPoint newTraverseData
        nextPointSignal =
          calculateSignal allRobots' nextPoint
        newRobotsToCross =
          L.filter
            (\rob -> distance nextPoint (pos rob) > r rob)
          robotsToCross'
    in if nextPoint == targetPos' then
         error "Implement reaching targetPoint!!"
       else
         quasiSimplexTraverse $
           newTraverseData
             { robotsToCross = newRobotsToCross
             , currentSignal = nextPointSignal
             , i = i td + 1
             }
  where
    -- assumption: this is always not empty, othewrise we are done
    robotsToCross' = robotsToCross td
    currentPoint' = currentPoint td
    allRobots' = allRobots td
    currentSignal' = currentSignal td
    maxSignal' = maxSignal td
    baseRobot' = baseRobot td
    targetPos' = targetPos td
    currentPointTargetDist =
      distance currentPoint' targetPos'

switchToNewTraversalDirection
  :: TraverseData
  -> Pos
  -> [Pos]
  -> TraverseData
switchToNewTraversalDirection td point neighbours =
  if distance newTargetPos point <= distance newTargetPos nextCandidatePos then
    error "distance to target will be increasing"
  else
    td
      { baseRobot = newBaseRobot
      , targetPos = newTargetPos
      , currentPoint = nextCandidatePos
      }
  where
    candidates =
      L.sortOn (((-1) *) . snd)
      $ L.map
          (\n -> (n, calculateSignal allRobots' n))
          neighbours
    (nextCandidatePos, nextCandidateSignal) =
      if candidates == [] then
        error "fix changing direction"
      else
        head candidates
    baseRobots =
      L.filter
        (\rob -> distance (pos rob) nextCandidatePos == r rob)
        allRobots'
    newBaseRobot =
      if baseRobots == [] then
        error "base robots is empty"
      else
        head baseRobots
    newTargets =
      L.sortOn
        (((-1) *) . distance nextCandidatePos . pos)
        (robotsToCross td)
    newTarget =
      if newTargets == [] then
        error "no targets to choose from!"
      else
        head newTargets
    newTargetPos = pos newTarget
    allRobots' = allRobots td

generateNeighboursInRange
  :: Distance
  -> Pos
  -> [Pos]
generateNeighboursInRange range pos@Pos { x = x', y = y', z = z' } =
  filter ((<= range) . distance pos)
  $ [ Pos x'' y'' z'' | x'' <- [x' - 1 .. x' + 1],
                        y'' <- [y' - 1 .. y' + 1],
                        z'' <- [z' - 1 .. z' + 1],
                        x'' /= x' || y'' /= y' || z'' /= z'
    ]

findPosClosestToPos0
  :: [Robot]
  -> Integer
  -> Pos
  -> Pos
findPosClosestToPos0
  robots
  maxSignal
  point =
  if nextPointCandidades == [] then
    point
  else
    findPosClosestToPos0
      robots
      maxSignal
      (L.head nextPointCandidades)
  where
    nextPointCandidades =
      L.filter
        (\n ->
          distance pos0 n < pointPos0Dist &&
          calculateSignal robots n >= maxSignal)
      $ neighbours
    pointPos0Dist = distance pos0 point
    neighbours =
      generateNeighboursInRange
        maxNeighbourhoodRange
        point

calculateSignal
  :: [Robot]
  -> Pos
  -> Integer
calculateSignal robots position =
  L.foldl'
    (\total rob ->
      if distance (pos rob) position <= r rob
      then 1 + total
      else total)
    0
    robots

parseNanobots
  :: String
  -> [Robot]
parseNanobots =
  map parseNanobot . lines

findRobotsInRadius
  :: [Robot]
  -> Robot
  -> [Robot]
findRobotsInRadius robots robot =
  filter (robotInRadius robot) robots

-- is rob2 in range of rob0
robotInRadius
  :: Robot
  -> Robot
  -> Bool
robotInRadius rob1@Robot { r = r1 } rob2 =
  robotsDistance rob1 rob2 <= r1

robotsDistance
  :: Robot
  -> Robot
  -> Integer
robotsDistance Robot { pos = pos1 } Robot { pos = pos2 } =
  distance pos1 pos2

distance
  :: Pos
  -> Pos
  -> Integer
distance pos1 pos2 =
  dx + dy + dz
  where
    dx = abs(x1-x2)
    dy = abs(y1-y2)
    dz = abs(z1-z2)
    Pos { x = x1, y = y1, z = z1 } = pos1
    Pos { x = x2, y = y2, z = z2 } = pos2

findRobotWithBiggestRadius
  :: [Robot]
  -> Robot
findRobotWithBiggestRadius =
  maximumBy (compare `F.on` r)

parseNanobot
  :: String
  -> Robot
parseNanobot input =
  let chunks     = words input
      posStr     = head chunks
      rStr       = head $ tail chunks
      (x0,y0,z0) = parsePos posStr
      r          = parseRadius rStr
  in  Robot (Pos x0 y0 z0) r
  where
    parsePos =
      (\[x,y,z] -> (x,y,z)) .
      map (read::String->Integer) .
      words .
      map (\c -> if c == ',' then ' ' else c) .
      reverse .
      drop 2 .
      reverse .
      drop 5
    parseRadius =
      (read::String->Integer) .
      drop 2

sample :: String
sample = "pos=<0,0,0>, r=4\npos=<1,0,0>, r=1\npos=<4,0,0>, r=3\npos=<0,2,0>, r=1\npos=<0,5,0>, r=3\npos=<0,0,3>, r=1\npos=<1,1,1>, r=1\npos=<1,1,2>, r=1\npos=<1,3,1>, r=1"

sampleB :: String
sampleB = "pos=<10,12,12>, r=2\npos=<12,14,12>, r=2\npos=<16,12,12>, r=4\npos=<14,14,14>, r=6\npos=<50,50,50>, r=200\npos=<10,10,10>, r=5"
