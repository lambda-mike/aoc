{-# LANGUAGE OverloadedStrings #-}

module Day20.Day20
  ( solve
  , solve2
  ) where

import Debug.Trace
import Data.Function (on)
import Data.List as L
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type Dir = Char -- N E S W
type Distance = Int
type Room = (Int, Int) -- (x, y)
type Path = (Room, Distance)

data TraversalState =
  TraversalState
    { baseMap :: Map Room Distance
    , directions :: Text
    , neighbours :: Map Room (Set Room)
    , nestingLevels :: Map Int [[Path]]
    , paths :: [Path]
    }
  deriving Show

data ShortestPathTree =
  ShortestPathTree
    { notVisitedRooms :: Set Room
    , previousRoom :: Map Room (Maybe Room) -- Nothing for starting pos
    , roomNeighbours :: Map Room (Set Room)
    , distances :: Map Room Distance -- shortest path to the room counted in number of door
    }
  deriving Show

startRoom = (0, 0)

solve2
  :: Text
  -> Int
solve2
  input
  = countRoomsAtLeastFar 1000 --door
  . generateShortestPathTree
  $ traverseWholeBase state
    where
      directions =
        getDirections input
      state =
        TraversalState
          { baseMap = M.singleton startRoom 0
          , directions = directions
          , neighbours = M.empty
          , nestingLevels = M.empty
          , paths = [(startRoom, 0)]
          }

solve
  :: Text
  -> Distance
solve
  input
  = getDoorNumberForFarestRoom
  . generateShortestPathTree
  $ traverseWholeBase state
    where
      directions =
        getDirections input
      state =
        TraversalState
          { baseMap = M.singleton startRoom 0
          , directions = directions
          , neighbours = M.empty
          , nestingLevels = M.empty
          , paths = [(startRoom, 0)]
          }

getDirections
  :: Text
  -> Text
getDirections =
  -- ignore ^ $ and \n
  T.tail . T.init . T.init

countRoomsAtLeastFar
  :: Int
  -> ShortestPathTree
  -> Int
countRoomsAtLeastFar door =
  M.size
  . M.filter (>= door)
  . distances

generateShortestPathTree
  :: TraversalState
  -> ShortestPathTree
generateShortestPathTree state =
  calculateAllShortestPathsDijkstra initialState
  where
    calculateAllShortestPathsDijkstra algState
      | 0 == S.size (notVisitedRooms algState) =
          algState
      | otherwise =
          let closestPath@(closestRoom, closestRoomDistance) =
                findClosestRoom algState
              newNotVisitedRooms =
                S.delete closestRoom (notVisitedRooms algState)
              closestRoomNeighbours =
                S.intersection
                  ((roomNeighbours algState) M.! closestRoom)
                  newNotVisitedRooms
          in calculateAllShortestPathsDijkstra
               (updateNeighbours
                 (algState
                   { notVisitedRooms = newNotVisitedRooms })
                 closestPath
                 closestRoomNeighbours)

    updateNeighbours
      :: ShortestPathTree
      -> Path -- current Room & its distance
      -> Set Room
      -> ShortestPathTree
    updateNeighbours spt (sourceRoom, sourceDist) roomNeighbours =
      S.foldr'
        (\neighbour algSt ->
          let distanceToNeighbour =
                1 + sourceDist -- rooms are always one door away
              recordedNeighbourDist =
                (distances algSt) M.! neighbour
          in if distanceToNeighbour < recordedNeighbourDist
             then algSt
                    { distances =
                        M.insert
                          neighbour
                          distanceToNeighbour
                          (distances algSt)
                    , previousRoom =
                        M.insert
                          neighbour
                          (Just sourceRoom)
                          (previousRoom algSt)
                    }
             else algSt)
        spt
        roomNeighbours

    findClosestRoom
      :: ShortestPathTree
      -> (Room, Distance)
    findClosestRoom
      ShortestPathTree
        { notVisitedRooms = notVisitedRooms'
        , distances = distances'
        } =
          S.foldr'
            (\room minSoFar@(_, minDist) ->
              let roomDist = distances' M.! room
              in if roomDist < minDist
                 then (room, roomDist)
                 else minSoFar
            )
            ((0, 0), maxBound::Int)
            notVisitedRooms'

    initialState =
      ShortestPathTree
        { notVisitedRooms = M.keysSet (baseMap state)
        , previousRoom = M.map (const Nothing) (baseMap state)
        , roomNeighbours = (neighbours state)
        , distances = (baseMap state)
        }

traverseWholeBase
  :: TraversalState
  -> TraversalState

traverseWholeBase state
  | "" == (directions state) =
    state
  | 'N' == (T.head . directions $ state) =
    traverseWholeBase $
      goToNextRoom 'N' state
  | 'E' == (T.head . directions $ state) =
    traverseWholeBase $
      goToNextRoom 'E' state
  | 'S' == (T.head . directions $ state) =
    traverseWholeBase $
      goToNextRoom 'S' state
  | 'W' == (T.head . directions $ state) =
    traverseWholeBase $
      goToNextRoom 'W' state
  | '(' == (T.head . directions $ state) =
    traverseWholeBase $
      branch state
  | '|' == (T.head . directions $ state) =
    traverseWholeBase $
      addCase state
  | ')' == (T.head . directions $ state) =
  -- | TODO remove duplicate rooms but with bigger door counter (loops)
    traverseWholeBase $
      closeBranch state
  | otherwise =
    error $ "Failed on case " <> show state

closeBranch
  :: TraversalState
  -> TraversalState
closeBranch state =
  let
    (latestBranchLevelKey, latestBranchLevelValue) =
      M.findMax (nestingLevels state)
    previousBranchIndex =
      latestBranchLevelKey - 1
    previousBranchLevel =
      (nestingLevels state) M.! previousBranchIndex
    finalPaths =
      L.nubBy ((==) `on` fst)
      . L.sortBy (compare `on` snd)
      . L.concat
      $ (paths state) : latestBranchLevelValue
  in
    advanceToNextDirection
    $ if 2 == M.size (nestingLevels state) then
        -- 0 & 1
        state
          { nestingLevels =
              M.empty
          , paths = finalPaths
          }
      else
        -- 2 and higher
        state
          { nestingLevels =
              M.updateMax
                (\ps ->
                  Just $
                    (paths state) : tail ps)
              . M.deleteMax
              $ (nestingLevels state)
          , paths = finalPaths
          }

addCase
  :: TraversalState
  -> TraversalState
addCase state =
  let
    (latestBranchLevelKey, latestBranchLevelValue) =
      M.findMax (nestingLevels state)
    previousBranchIndex =
      latestBranchLevelKey - 1
    previousBranchLevel =
      (nestingLevels state) M.! previousBranchIndex
  in
    advanceToNextDirection
    $ state
      { nestingLevels =
          M.adjust
            ((paths state) :)
            latestBranchLevelKey
            (nestingLevels state)
      , paths = head previousBranchLevel
      }

-- only for (
branch
  :: TraversalState
  -> TraversalState
branch state =
  advanceToNextDirection
  $ state
    { nestingLevels =
        M.insert
          nestingLevel
          [] -- empty list ready for adding snapshots of paths
        $ if M.empty == nestingLevels state then
            M.insert
              0
              [paths state]
              M.empty
          else
            M.updateMax
              (Just . ((paths state) :))
              (nestingLevels state)
    }
  where
    -- 1, 2, 3...
    nestingLevel =
      if M.empty == nestingLevels state
      then 1
      else (M.size . nestingLevels $ state)

goToNextRoom
  :: Dir
  -> TraversalState
  -> TraversalState
goToNextRoom dir state =
  let newPaths = map (move dir) (paths state)
      newNeighboursChunk =
        zipWith
          ((,) `on` fst)
          (paths state)
          newPaths
      symmetricNewNeighboursChunk =
        concatMap
          (\(r1, r2) -> [(r1, S.singleton r2), (r2, S.singleton r1)])
          newNeighboursChunk
  in  advanceToNextDirection
    $ state
        { baseMap =
            L.foldl'
              (flip upsertMaxDistance)
              (baseMap state)
              newPaths
        , paths = newPaths
        , neighbours =
            M.unionWith
              S.union
              (M.fromList symmetricNewNeighboursChunk)
              (neighbours state)
        }

upsertMaxDistance
  :: Path
  -> Map Room Distance
  -> Map Room Distance
upsertMaxDistance (room, dist) =
  M.insertWith
    (flip const) -- ignore new value
    room
    (maxBound::Int)

getDoorNumberForFarestRoom
  :: ShortestPathTree
  -> Distance
getDoorNumberForFarestRoom =
  M.foldr' max 0 . distances

move
  :: Dir
  -> Path
  -> Path
move 'N' ((x, y), _) = ((x, 1 + y), maxBound::Int)
move 'E' ((x, y), _) = ((1 + x, y), maxBound::Int)
move 'S' ((x, y), _) = ((x, y - 1), maxBound::Int)
move 'W' ((x, y), _) = ((x - 1, y), maxBound::Int)

advanceToNextDirection
  :: TraversalState
  -> TraversalState
advanceToNextDirection state =
  state
    { directions = T.tail . directions $ state
    }

