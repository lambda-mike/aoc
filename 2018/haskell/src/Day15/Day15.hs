module Day15.Day15
  ( Battle(..)
  , Cell(..)
  , Unit(..)
  , solve
  , solveB
  , combat1
  , combat2
  , combat3
  , combat4
  , combat5
  , combat1b
  , combat2b
  , combat3b
  , combat4b
  , combat5b
  -- TEST
  , initialPower
  , parseBattlefield
  , initBattle
  , isBattleOver
  , calculateOutcome
  , orderUnits
  , findAdjacentEnemy
  , move
  , findTargets
  ) where

import Data.Function ((&), on)
import Data.Maybe (catMaybes)
import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Tuple (swap)
import Prelude hiding (round)

-- the number of full rounds that were completed
-- (not counting the round in which combat ends)
-- multiplied by the sum of the hit points of all
-- remaining units at the moment combat ends.
type Outcome = Int

type Pos = (Int, Int) -- x, y

data Cell
  = Cavern
  | Wall
  | Elf Unit
  | Goblin Unit
  deriving (Eq, Show)

data Unit
  = Unit
  { power :: Int
  , hitPoints :: Int
  , position :: Pos
  }
  deriving (Eq, Show)

type Battlefield =
  Map Pos Cell

data Battle
  = Battle
  { battlefield :: Battlefield
  , round :: Int
  }
  deriving (Show)

data Experiment
  = Experiment
  { battlefieldRaw :: String
  , currentElfPower :: Int
  , battle :: Battle
  , elvesNum :: Int
  }

solveB
  :: String
  -> Outcome
solveB
  = calculateOutcome
  . battle
  . findBattleAllElvesAlive
  . initExperiment initialPower

solve
  :: String
  -> Outcome
solve
  = calculateOutcome
  . fight
  . initBattle
  . parseBattlefield initialPower

calculateOutcome
  :: Battle
  -> Outcome
calculateOutcome battle =
  rounds * totalHitPoints
  where
    rounds =
      round battle
    totalHitPoints =
      battlefield battle
      & M.foldl' (\s cell -> s + getHitPoints cell) 0
    getHitPoints u =
      case u of
        Cavern -> 0
        Wall -> 0
        Elf (Unit { hitPoints = hp }) -> hp
        Goblin (Unit { hitPoints = hp }) -> hp

findBattleAllElvesAlive
  :: Experiment
  -> Experiment
findBattleAllElvesAlive ex =
  if noGoblins && allElvesAlive then
    ex
  else
    let newElfPower = currentElfPower ex + 1
    in findBattleAllElvesAlive
      ex { currentElfPower =
             newElfPower
         , battle =
            fight $
              initBattle $
                parseBattlefield newElfPower (battlefieldRaw ex)
         }
  where
    Battle { battlefield = battlefield' } = battle ex
    elvesSurvivors =
      (M.size . M.filter isElf) battlefield'
    noGoblins =
      M.null (M.filter isGoblin battlefield')
    allElvesAlive =
      elvesNum ex == elvesSurvivors

initExperiment
  :: Int
  -> String
  -> Experiment
initExperiment elfPower input
  = Experiment
  { battlefieldRaw = input
  , battle =
      initBattle $
        parseBattlefield elfPower input
  , currentElfPower = elfPower
  , elvesNum = length $ filter (== 'E') input
  }

fight
  :: Battle
  -> Battle
fight battle
  | isBattleOver battle = battle
  | otherwise =
    let (hasRoundEndedPrematurely, newBattle) =
         L.foldl'
            runTurn
            (False, battlefield')
            (orderUnits battle)
    in fight $ Battle
       { round =
           round battle +
            -- count only full rounds
            if hasRoundEndedPrematurely
            then 0
            else 1
       , battlefield =
           newBattle

       }
  where
    battlefield' = battlefield battle

isBattleOver
  :: Battle
  -> Bool
isBattleOver battle =
  onlyOneArmyLeft $
    battlefield battle

onlyOneArmyLeft
  :: Battlefield
  -> Bool
onlyOneArmyLeft battlefield' =
  doesNotContain isElf battlefield'
  ||
  doesNotContain isGoblin battlefield'
  where
    doesNotContain pred
      = (== 0)
      . M.size
      . M.filter pred

isGoblin
  :: Cell
  -> Bool
isGoblin (Goblin _) = True
isGoblin _ = False

isElf
  :: Cell
  -> Bool
isElf (Elf _) = True
isElf _ = False

runTurn
  :: (Bool, Battlefield)
  -> Cell
  -> (Bool, Battlefield)
runTurn (hasEndedPrematurely, battlefield') cellFromPrevBattle =
  if isElf cell || isGoblin cell then
    case findAdjacentEnemy cell battlefield' of
      Nothing ->
        let (newCell, newBattlefield) = move cell battlefield'
        in  if newCell == cell
            then (newHasEndedPrematurely, newBattlefield)
            else maybe
                   (newHasEndedPrematurely, newBattlefield)
                   (\enemy ->
                     ( newHasEndedPrematurely
                     , attack newBattlefield newCell enemy
                     )
                   )
                   (findAdjacentEnemy newCell newBattlefield)
      Just enemy ->
        (hasEndedPrematurely, attack battlefield' cell enemy)
  else
    (newHasEndedPrematurely, battlefield')
  where
    cell =
      battlefield' M.! (position . getUnit $ cellFromPrevBattle)
    newHasEndedPrematurely =
      onlyOneArmyLeft battlefield' || hasEndedPrematurely

findAdjacentEnemy
  :: Cell
  -> Battlefield
  -> Maybe Cell
findAdjacentEnemy cell battlefield' =
  case cell of
    Elf unit ->
      findEnemy isGoblin unit battlefield'
    Goblin unit ->
      findEnemy isElf unit battlefield'
    c -> error $ (show c) <> " does not have enemies, silly!"
  where
    findEnemy enemyPred unit battlefield' =
      let pos = position unit
          adjacentPositions =
            getAdjacentPositions pos
          weakestEnemyOrFirstInReadingOrder
            = take 1
            . L.sortBy (compare `on` (createIndex . getUnit))
            . L.filter enemyPred
            . catMaybes
            . L.map (flip M.lookup battlefield')
            $ adjacentPositions
      in  case weakestEnemyOrFirstInReadingOrder of
            [] -> Nothing
            c : _ -> Just c
    createIndex unit =
      (hitPoints unit, swap $ position unit)

getAdjacentPositions
  :: Pos
  -> [Pos]
getAdjacentPositions (x, y) =
  [ (x, y-1)
  , (x-1, y)
  , (x+1, y)
  , (x, y+1)
  ]

getUnit
  :: Cell
  -> Unit
getUnit (Elf u)  = u
getUnit (Goblin u) = u
getUnit u = error $ "you can only get unit for goblin or elf! " <> show u

move
  :: Cell
  -> Battlefield
  -> (Cell, Battlefield)
move cell battlefield' =
  let nextPos =
        findNextPos
          cell
          battlefield'
  in maybe
       (cell, battlefield')
       (step battlefield' cell)
       nextPos

findNextPos
  :: Cell
  -> Battlefield
  -> Maybe Pos
findNextPos cell battlefield' =
  let targets =
        findTargets cell battlefield'
      graph =
        M.filter (== Cavern) battlefield'
      start =
        position $ getUnit cell
      queue =
        M.insert start cell $
          graph
      distancesInit =
        M.insert start (Just 0) $
          M.map (const Nothing) graph
      pathsInit =
        M.insert start Nothing $
          M.map (const Nothing) graph
      (distances, paths) =
        dijkstra
          targets
          graph
          queue
          distancesInit
          pathsInit
      targetsDistances =
        targets
        & M.mapWithKey (\pos _ -> distances M.! pos)
      closestTarget =
        M.foldlWithKey
          (\result pos mdist ->
            case (mdist, result) of
              (Just dist, Just (rpos, rdist)) ->
                if dist < rdist then
                  Just (pos, dist)
                else
                  if dist == rdist then
                    if swap pos < swap rpos then
                     Just (pos, dist)
                    else
                     Just (rpos, rdist)
                  else
                     Just (rpos, rdist)
              (Just dist, Nothing) ->
                Just (pos, dist)
              _ -> result
          )
          Nothing
          targetsDistances
  in getFirstStep paths start
     . fst
     <$> closestTarget

findTargets
  :: Cell
  -> Battlefield
  -> Map Pos Cell
findTargets cell battlefield' =
  let enemyCheck =
        if isElf cell
        then isGoblin
        else isElf
      enemies =
        M.filter enemyCheck battlefield'
  in M.foldl addAdjacentCaverns M.empty enemies
  where
    addAdjacentCaverns caverns enemy =
      let enemyPos = position $ getUnit enemy
          neighbours = getAdjacentPositions enemyPos
          adjacentCaverns =
            L.filter
              ((Cavern ==) . (battlefield' M.!))
              neighbours
      in L.foldl
           (\acc cavernPos ->
             M.insert cavernPos Cavern acc)
           caverns
           adjacentCaverns

dijkstra
  :: Map Pos Cell
  -> Map Pos Cell
  -> Map Pos Cell
  -> Map Pos (Maybe Int)
  -> Map Pos (Maybe Pos) -- single pos is enough? what about multiple short paths?
  -> (Map Pos (Maybe Int), Map Pos (Maybe Pos))
dijkstra
  targets
  graph
  queue
  distances
  paths =
  -- do not continue, because all other cells are further than targets
  if M.empty == queue || M.empty == targets then
  -- if M.empty == queue then
    (distances, paths)
  else
    let (minPos, minDist) =
          findMinDist $
            M.intersection
              distances
              queue
        newQueue =
          -- there is no minimal
          if minDist == Nothing then
            M.empty
          else
            M.delete minPos queue
        newTargets =
          M.delete minPos targets
        minPosNeighbours =
          getNeighboursPos
            graph
            minPos
            -- newQueue -- in original alg we need only neighbours which are still in queue
            -- probably we need all neightbours, because some paths might occur earlier in reading order
        (newDistances, newPaths) =
          traverseNeighbours
            minPos
            minDist
            minPosNeighbours
            distances
            paths

    in dijkstra
         newTargets
         graph
         newQueue
         newDistances
         newPaths
  where
    findMinDist
      :: Map Pos (Maybe Int)
      -> (Pos, Maybe Int)
    findMinDist =
      M.foldlWithKey'
        (\(minSoFarPos, minSoFarDist) pos dist ->
          case (minSoFarDist, dist) of
            (Just msfd, Just d) ->
              if d < msfd
              then (pos, dist)
              else (minSoFarPos, minSoFarDist)
            (Just msfd, Nothing) ->
              (minSoFarPos, minSoFarDist)
            (Nothing, _) ->
              (pos, dist)
        )
        ((-1, -1), Nothing)
    getNeighboursPos
      :: Map Pos Cell
      -> Pos
      -> [Pos]
    getNeighboursPos graph pos =
      M.keys
      . M.intersection graph
      $ M.fromList $
          L.zip
            (getAdjacentPositions pos)
            (repeat Cavern)
    traverseNeighbours
      :: Pos
      -> Maybe Int
      -> [Pos]
      -> Map Pos (Maybe Int)
      -> Map Pos (Maybe Pos)
      -> (Map Pos (Maybe Int), Map Pos (Maybe Pos))
    traverseNeighbours
      minPos
      minDist
      neighbours
      distances
      paths =
        if minDist == Nothing then
          (distances, paths)
        else
          case neighbours of
            [] ->
              (distances, paths)
            neighbourPos:neighboursPos ->
              let neighbourDistSoFar =
                    distances M.! neighbourPos
                  neighbourDistFromMinPos =
                    (+ 1) <$> minDist
                  neighbourPrevPos =
                    paths M.! neighbourPos
                  (newNeighbourDist, newNeighbourPrevPos) =
                    if neighbourDistSoFar == Nothing
                    || neighbourDistFromMinPos < neighbourDistSoFar then
                      (neighbourDistFromMinPos, Just minPos)
                    else if neighbourDistFromMinPos == neighbourDistSoFar then
                      if neighbourPrevPos == Nothing then
                        (neighbourDistFromMinPos, Just minPos)
                      else if Just (swap minPos) < (swap <$> neighbourPrevPos) then
                        (neighbourDistFromMinPos, Just minPos)
                      else
                        (neighbourDistFromMinPos, neighbourPrevPos)
                    else
                      (neighbourDistSoFar, neighbourPrevPos)
              in traverseNeighbours
                   minPos
                   minDist
                   neighboursPos
                   (M.insert neighbourPos newNeighbourDist distances)
                   (M.insert neighbourPos newNeighbourPrevPos paths)

getFirstStep
  :: Map Pos (Maybe Pos)
  -> Pos
  -> Pos
  -> Pos
getFirstStep paths start pos
  | paths M.! pos == Just start
    = pos
  | otherwise
    = case (paths M.! pos) of
        Nothing ->
          error ("incomplete path for " <> show pos)
        Just p ->
          getFirstStep paths start p

step
  :: Battlefield
  -> Cell
  -> Pos
  -> (Cell, Battlefield)
step battlefield' cell pos =
-- check if cell is no more than one step in cross-direction N W S E
-- update elf/goblin, put cavern in their position
  let cellPos = position . getUnit $ cell
      cellNeighbours = getAdjacentPositions cellPos
  in if any (== pos) cellNeighbours then
       case cell of
         Elf u ->
           stepCellUpdateBattlefield Elf u
         Goblin u ->
           stepCellUpdateBattlefield Goblin u
         _ ->
           error ("You must not step " <> show cell)
     else
       error ("Must not move cell" <> show cellPos <>
             " to position " <> show pos)
  where
    stepCellUpdateBattlefield unitTypeConstructor unit =
      let newCell =
            unitTypeConstructor (unit { position = pos })
          newBattlefield =
            M.insert (position unit) Cavern $
              M.insert pos newCell battlefield'
      in (newCell, newBattlefield)

attack
  :: Battlefield
  -> Cell
  -> Cell
  -> Battlefield
attack battlefield' attacker target =
  let damage = power $ getUnit attacker
      targetUnit = getUnit target
      targetHP = hitPoints targetUnit
      targetPos = position targetUnit
      targetUnitType = if isElf target then Elf else Goblin
      attackedTargetHP = targetHP - damage
  in
    if attackedTargetHP <= 0 then
      M.insert targetPos Cavern battlefield'
    else
      M.insert
        targetPos
        (targetUnitType (targetUnit { hitPoints = attackedTargetHP }))
        battlefield'

orderUnits
  :: Battle
  -> [Cell]
orderUnits
  = L.map snd
  . L.sortBy readigOrder
  . M.toList
  . M.filter (\c -> isElf c || isGoblin c)
  . battlefield
  where
    readigOrder =
      compare `on` (swap . fst)

initBattle
  :: Battlefield
  -> Battle
initBattle battlefield =
  Battle
    { battlefield = battlefield
    , round = 0
    }

parseBattlefield
  :: Int
  -> String
  -> Battlefield
parseBattlefield elfPower
  = M.fromList
  . concatMap (\(y, xcs) ->
      map
        (\(x, c) -> ((x, y), parseCell elfPower (x,y) c))
        xcs)
  . zip ys
  . map (zip xs)
  . lines
  where
    xs = [0..]
    ys = [0..]

parseCell
  :: Int
  -> Pos
  -> Char
  -> Cell

parseCell _ _ '#' = Wall
parseCell _ _ '.' = Cavern
parseCell elfPower pos 'E' = Elf $
  Unit { power = elfPower, hitPoints = initialHP, position = pos }
parseCell _ pos 'G' = Goblin $
  Unit { power = initialPower, hitPoints = initialHP, position = pos }

initialPower :: Int
initialPower = 3

initialHP :: Int
initialHP = 200

combat1 :: String
combat1 = "#######\n#G..#E#\n#E#E.E#\n#G.##.#\n#...#E#\n#...E.#\n#######"

combat2 :: String
combat2 = "#######\n#E..EG#\n#.#G.E#\n#E.##E#\n#G..#.#\n#..E#.#\n#######"

combat3 :: String
combat3 = "#######\n#E.G#.#\n#.#G..#\n#G.#.G#\n#G..#.#\n#...E.#\n#######"

combat4 :: String
combat4 = "#######\n#.E...#\n#.#..G#\n#.###.#\n#E#G#G#\n#...#G#\n#######"

combat5 :: String
combat5 = "#########\n#G......#\n#.E.#...#\n#..##..G#\n#...##..#\n#...#...#\n#.G...G.#\n#.....G.#\n#########"

combat1b :: String
combat1b = "#######\n#.G...#\n#...EG#\n#.#.#G#\n#..G#E#\n#.....#\n#######"

combat2b :: String
combat2b = "#######\n#E..EG#\n#.#G.E#\n#E.##E#\n#G..#.#\n#..E#.#\n#######"

combat3b :: String
combat3b = "#######\n#E.G#.#\n#.#G..#\n#G.#.G#\n#G..#.#\n#...E.#\n#######"

combat4b :: String
combat4b = "\n#######\n#.E...#\n#.#..G#\n#.###.#\n#E#G#G#\n#...#G#\n#######"

combat5b :: String
combat5b = "#########\n#G......#\n#.E.#...#\n#..##..G#\n#...##..#\n#...#...#\n#.G...G.#\n#.....G.#\n#########"
