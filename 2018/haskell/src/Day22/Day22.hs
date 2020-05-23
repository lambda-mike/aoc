{-# LANGUAGE BangPatterns #-}

module Day22.Day22 where

import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU


type Depth = Int
type Risk = Int
type GeoIndex = Int
type ErosionLevel = Int
type X = Int
type Y = Int

data Equipment
  = None
  | Torch
  | Gear
  deriving (Eq, Ord, Show)

-- 0 rocky; 1 wet; 2 narrow
type Region = Int
data Node =
  Node
    { x :: !X
    , y :: !Y
    , eq :: !Equipment
    }
  deriving (Eq, Ord, Show)
type Time = Int
type CaveGraph = M.Map Node Time
type ErosionV = VU.Vector ErosionLevel
type RegionV = VU.Vector Region

data Cave
  = Cave
  { erosionData :: !ErosionV
  , regionData :: !RegionV
  , depth :: !Depth
  , revealDepth :: !Depth
  -- all nodes in cave graph - both valid and invalid
  -- (all not visited nodes have either plus inf or will be in time dist and cluster
  , notVisitedNodes :: !CaveGraph
  -- map of times and not yet visited nodes which have that time value currently set
  , nodesTimeCluster :: !(Map Time (Set Node))
  , target :: !Node
  -- map of times per nodes
  , timeDist :: !CaveGraph
  }
  deriving Show

data MinNodeResult =
  MinNodeResult
    { node :: !Node
    , t :: !Time
    , cave :: !Cave
    }

data NeighboursResult =
  NeighboursResult
    { nodes :: ![Node]
    , caveN :: !Cave
    }


input :: String
input = "depth: 11109\ntarget: 9,731"

geoX :: Int
geoX = 16807

geoY :: Int
geoY = 48271

erosionConst :: Int
erosionConst = 20183

rocky :: Int
rocky = 0

wet :: Int
wet = 1

narrow :: Int
narrow = 2

unknownErosionLevel :: ErosionLevel
unknownErosionLevel = -1

solveA
  :: String
  -> Risk
solveA =
  calculateCaveRiskLevel
  . parseInput

solveB
  :: String
  -> Time
solveB =
  findShortestTimeToTarget
  . initializeCave
  . parseInput

findShortestTimeToTarget :: Cave -> Time
findShortestTimeToTarget cave =
  let result = dijkstra cave'
  in timeDist result M.! target'
  where
    depth' = depth cave
    target' = target cave
    caveMouth = Node 0 0 Torch
    cave' =
      cave
        { notVisitedNodes =
            M.fromList
              [ (caveMouth, 0)
              , ((Node 0 0 Gear), 0)
              ]
        , nodesTimeCluster =
            M.singleton 0 (S.singleton caveMouth)
        , timeDist =
            M.singleton caveMouth 0
        }

dijkstra :: Cave -> Cave
dijkstra cave =
  if M.empty == notVisitedNodes' && revealedDepth == depth'
  then
    error
      "could not find a shortest path before exhausting all the nodes in the graph!"
  else if minNode == target'
    then cave
    else dijkstra newCave
  where
    notVisitedNodes' = notVisitedNodes cave
    MinNodeResult { node = minNode, t = minNodeTime, cave = cave' } =
      getMinNode cave
    NeighboursResult { nodes = neighbours, caveN = cave'' } =
      getNodeNeighbours cave' minNode
    newCave =
      L.foldl'
        (updateDistances minNode minNodeTime)
        cave''
        neighbours
    revealedDepth = revealDepth cave
    target' = target cave
    depth' = depth cave

-- update dist of neighbour if new values are smaller
-- update times cluster as well
-- remove node from notVisited if its first distance
-- clear graph from invalid nodes
updateDistances
  :: Node
  -> Time
  -> Cave
  -> Node
  -> Cave
updateDistances minNode minNodeTime cave neighbour =
  let prevNeighbourTimeDistance =
        M.lookup
          neighbour
          (timeDist cave)
      altNeighbourTimeDistance =
        calculateNodeDistance
          minNode
          minNodeTime
          neighbour
      newCave =
        updateNodeDistance
          neighbour
          altNeighbourTimeDistance
          cave
  in case prevNeighbourTimeDistance of
    Just t ->
      if altNeighbourTimeDistance < t then
        addTimeDistanceToCluster
          neighbour
          altNeighbourTimeDistance
          (removePrevTimeDistanceFromCluster
            neighbour
            t
            newCave)
      else
        newCave

    Nothing ->
      addTimeDistanceToCluster
        neighbour
        altNeighbourTimeDistance
        newCave

calculateNodeDistance
  :: Node
  -> Time
  -> Node
  -> Time
calculateNodeDistance minNode minNodeTime neighbour =
  minNodeTime + distance
  where
    distance =
      if nx == minX && ny == minY
      then 7 -- seven minutes for switching gear
      else 1 -- one minute for moving to cell in neighbourhood
    minX = x minNode
    minY = y minNode
    nx = x neighbour
    ny = y neighbour

-- update time value for node in timeDist map
updateNodeDistance
  :: Node
  -> Time
  -> Cave
  -> Cave
updateNodeDistance node time cave =
  cave
    { timeDist =
        M.alter
          (const $ Just time)
          node
          (timeDist cave) }

addTimeDistanceToCluster
  :: Node
  -> Time
  -> Cave
  -> Cave
addTimeDistanceToCluster node time cave =
  cave
    { nodesTimeCluster =
        M.alter
          (\ns -> case ns of
            Just nss ->
              Just $ S.insert node nss
            Nothing ->
              Just $ S.singleton node
          )
          time
          (nodesTimeCluster cave)
    }

removePrevTimeDistanceFromCluster
  :: Node
  -> Time
  -> Cave
  -> Cave
removePrevTimeDistanceFromCluster node time cave =
  cave
    { nodesTimeCluster =
        M.alter
          (S.delete node <$>)
          time
          (nodesTimeCluster cave)
    }

-- finds smallest min node and removes from the cluster and not visited node
getMinNode
  :: Cave
  -> MinNodeResult
getMinNode cave =
  -- find nodes with min time; assume it is always vaild region for that pos
  case M.lookupMin nodesTimeCluster' of
    Just (t, nodes) ->
      -- take first node from set with min time key
      let (single, rest) = S.splitAt 1 nodes
          node = S.elemAt 0 single
          newCluster =
            -- if no more nodes in set
            if rest == S.empty
            -- delete record for t altogether
            then M.delete t nodesTimeCluster'
            -- otherwise just update set with the node removed from it
            else M.adjust (const rest) t nodesTimeCluster'
          newCave =
            markNodeVisited node $
              cave { nodesTimeCluster = newCluster }
      in MinNodeResult
          { node = node
          , t = t
          , cave = newCave
          }
    Nothing ->
      error "Is it possible? No nodes in the time cluster?"
  where
    nodesTimeCluster' = nodesTimeCluster cave

cleanNotVisitedNodes
  :: Node
  -> Cave
  -> Cave
cleanNotVisitedNodes node cave =
  cave { notVisitedNodes =
    M.delete node (notVisitedNodes cave) }

markNodeVisited
  :: Node
  -> Cave
  -> Cave
markNodeVisited node cave =
  cave
    { notVisitedNodes =
        M.delete node (notVisitedNodes cave) }

-- Gets all neighbours of the node which are still not visited
-- cleans the graph from invalid versions as well
getNodeNeighbours
  :: Cave
  -> Node
  -> NeighboursResult
getNodeNeighbours cave node =
  let x' = x node
      y' = y node
      nodeRegion = getNodeRegion cave (x', y')
      equipment = eq node
      nodeAlternativeEquipment =
        case (nodeRegion, equipment) of
          (0, Torch) -> Gear
          (0, Gear) -> Torch
          (1, Gear) -> None
          (1, None) -> Gear
          (2, None) -> Torch
          (2, Torch) -> None
      compatibleRegions =
        case equipment of
          None -> [wet,narrow]
          Torch -> [rocky,narrow]
          Gear -> [rocky,wet]
      revealedDepth = revealDepth cave
      expandedCave =
        if x' + 1 >= revealedDepth ||
           y' + 1 >= revealedDepth
        then reveal cave
        else cave
      adjacentRegions =
        (x', y', nodeRegion) : getAdjacentRegions expandedCave node
      compatibleNeighbours =
        L.filter
          (\(_, _, region) ->
            L.any (== region) compatibleRegions)
          adjacentRegions
      compatibleNeighboursNodes =
        L.map
          (\(nx, ny, _) ->
            if x' == nx && y' == ny
            then (Node nx ny nodeAlternativeEquipment)
            else (Node nx ny equipment)
          )
          compatibleNeighbours
      notVisitedNodes' =
        notVisitedNodes expandedCave
  in NeighboursResult
      { nodes =
          L.filter
           (flip M.member notVisitedNodes')
           compatibleNeighboursNodes
      , caveN = expandedCave
      }

getIncompatibleEquipment
  :: Region
  -> Equipment
getIncompatibleEquipment region =
  case region of
    0 -> None
    1 -> Torch
    2 -> Gear

getAdjacentRegions
  :: Cave
  -> Node
  -> [(X, Y, Region)]
getAdjacentRegions cave node@(Node { x = x', y = y' }) =
  L.concatMap
      (map
        (\(xx, yy) ->
          (xx, yy, getNodeRegion cave (xx, yy ))))
    [north, east, south, west]
  where
    north =
      if y' == 0
      then []
      else [(x', y' - 1)]
    east =
      if x' + 1 == depth'
      then []
      else [(x' + 1, y')]
    south =
      if y' + 1 == depth'
      then []
      else [(x', y' + 1)]
    west =
      if x' == 0
      then []
      else [(x' - 1, y')]
    depth' = depth cave

getNodeRegion
  :: Cave
  -> (X, Y)
  -> Region
getNodeRegion cave (x', y') =
  (regionData cave) VU.! (depth cave * y' + x')

initializeCave :: (Depth, (X, Y)) -> Cave
initializeCave (depth, target) = Cave
  { erosionData =
      (VU.replicate caveSize unknownErosionLevel) -- unknown erosion
        VU.// [(0, caveMouthErosion)]
  , regionData =
      (VU.replicate caveSize unknownErosionLevel) -- unknown region
        VU.// [(0, caveMouthRegion)]
  , depth = depth
  , notVisitedNodes = M.empty
  , nodesTimeCluster = M.empty
  , revealDepth = 1 -- only mouth of the cell
  , target = Node (fst target) (snd target) Torch
  , timeDist = M.empty
  }
  where
    caveSize = depth * depth
    caveMouthErosion =
      -- geoIndex in cave mouth is always 0
      calculateErosionLevel depth 0
    caveMouthRegion =
      calculateRegion caveMouthErosion

reveal :: Cave -> Cave
reveal cave =
  if newRevealDepth <= depth cave then
    populateNotVisitedNodes $
      populateCaveRow $
        populateCaveColumn $
          cave {
            revealDepth = newRevealDepth
          }
  else
    cave
  where
    newRevealDepth = revealDepth cave + 1

populateNotVisitedNodes
  :: Cave
  -> Cave
populateNotVisitedNodes cave =
  cave {
    notVisitedNodes =
      M.unions
        [ notVisitedNodes'
        , M.fromList newNodesFromCol
        , M.fromList newNodesFromRow
        ]
    }
  where
    notVisitedNodes' = notVisitedNodes cave
    -- because it was incremented in reveal fn
    latestIndex = revealDepth cave - 1
    fullEquipment =
      [None, Torch, Gear]
    getIncompatibleEquipment' x' y' =
      getIncompatibleEquipment $
        getNodeRegion
          cave
          (x', y')
    newNodesFromCol =
      [((Node latestIndex y' e), maxBound::Int)
        | y' <- [0..latestIndex - 1], -- one cell less to avoid duplication
          e <-
            L.filter
              (getIncompatibleEquipment' latestIndex y' /=)
              fullEquipment
      ]
    newNodesFromRow =
      [((Node x' latestIndex e), maxBound::Int)
        | x' <- [0..latestIndex],
          e <-
            L.filter
              (getIncompatibleEquipment' x' latestIndex /=)
              fullEquipment
      ]

populateCaveColumn
  :: Cave
  -> Cave
populateCaveColumn cave =
  populateCaveAlongAxis
     cave
     (revealDepth' - 1)
     cellUpdater
  where
    cellUpdater v =
      let row =
            VU.length v
          i =
            row * depth' + col
          upErosion =
            if VU.null v
            then 0
            else snd $ VU.last v
          leftCellErosion =
            erosionLeft oldErosionData depth' (col, row)
          erosionLevel =
            calculateErosionLevel depth' $
              calculateGeologicIndex
                (x target', y target')
                (col, row)
                upErosion
                leftCellErosion
      in  (i, erosionLevel)
    depth' = depth cave
    revealDepth' = revealDepth cave
    col = revealDepth' - 1
    oldErosionData = erosionData cave
    target' = target cave

populateCaveRow
  :: Cave
  -> Cave
populateCaveRow cave =
  populateCaveAlongAxis
     cave
     revealDepth'
     cellUpdater
  where
    cellUpdater v =
      let col =
            VU.length v
          i =
            row * depth' + col
          leftCellErosion =
            if VU.null v
            then 0
            else snd $ VU.last v
          upErosion =
            erosionUp oldErosionData depth' (col, row)
          erosionLevel =
            calculateErosionLevel depth' $
              calculateGeologicIndex
                (x target', y target')
                (col, row)
                upErosion
                leftCellErosion
      in  (i :: Int, erosionLevel)
    depth' = depth cave
    revealDepth' = revealDepth cave
    row = revealDepth' - 1
    oldErosionData = erosionData cave
    target' = target cave

populateCaveAlongAxis
  :: Cave
  -> Int
  -> (VU.Vector (Int, ErosionLevel) -> (Int, ErosionLevel))
  -> Cave
populateCaveAlongAxis cave n cellConstructor =
  if revealDepth' > depth' then
    cave
  else
    cave
      { erosionData = erosions
      , regionData = regions
      }
  where
    regions =
      oldRegionData `VU.update`
        (VU.map
          (\(i, e) -> (i, calculateRegion e))
          updateVector)
    erosions =
      oldErosionData `VU.update` updateVector
    updateVector =
      VU.constructN n cellConstructor
    depth' = depth cave
    revealDepth' = revealDepth cave
    oldErosionData = erosionData cave
    oldRegionData = regionData cave

calculateRegion :: ErosionLevel -> Region
calculateRegion = (`mod` 3)

parseInput
  :: String
  -> (Depth, (X, Y))
parseInput str =
  (read depthStr :: Int, (read x :: Int, read y :: Int))
  where
    _ : depthStr : _ = L.words line1
    x = L.takeWhile (/= ',') coordsStr
    y = L.tail $ L.dropWhile (/= ',') coordsStr
    _ : coordsStr : _ = L.words line2
    (line1 : line2 : _) =
      L.lines str

calculateErosionLevel :: Depth -> GeoIndex -> ErosionLevel
calculateErosionLevel depth geoIndex =
  (geoIndex + (fromIntegral depth)) `mod` erosionConst

calculateGeologicIndex
  :: (X, Y)
  -> (X, Y)
  -> ErosionLevel
  -> ErosionLevel
  -> GeoIndex
calculateGeologicIndex _    (0, 0) _ _ = 0
calculateGeologicIndex _    (x, 0) _ _ = (fromIntegral x) * geoX
calculateGeologicIndex _    (0, y) _ _ = (fromIntegral y) * geoY
calculateGeologicIndex (tx, ty) cell@(x, y) upErosion leftErosion =
  if tx == x && ty == y then
    0
  else
    leftErosion * upErosion

erosionUp :: ErosionV -> Depth -> (X, Y) -> ErosionLevel
erosionUp terrain' depth (x', y') =
  if 0 == y'
  then error "no cell above first row"
  else terrain' VU.! (getIndex depth (x', y'-1))

erosionLeft :: ErosionV -> Depth -> (X, Y) -> ErosionLevel
erosionLeft terrain' depth (x', y') =
  if 0 == x'
  then error "no cell before first column"
  else terrain' VU.! (getIndex depth (x'-1, y'))

getIndex :: Depth -> (X, Y) -> Int
getIndex depth' (x', y') =
  y' * depth' + x'

calculateRisk :: ErosionLevel -> Risk
calculateRisk = (`mod` 3)

calculateCaveRiskLevel
  :: (Depth, (X, Y))
  -> Risk
calculateCaveRiskLevel (depth', target@(tx, ty)) =
  go
    initialPrevErosionRow
    initialErosionRow
    (0, 0)
    0
  where
    initialPrevErosionRow =
      VU.empty
    initialErosionRow =
      VU.replicate (tx + 1) 0
    go
      :: VU.Vector Int
      -> VU.Vector Int
      -> (Int, Int)
      -> Risk
      -> Risk
    go
      prevErosionRow
      currentErosionRow
      (x, y)
      riskSum
      | y > ty =
        riskSum
      | x > tx =
        go
          currentErosionRow
          initialErosionRow
          (0, y + 1)
          riskSum
      | otherwise =
        let upErosion =
              prevErosionRow VU.! x
            leftErosion =
              currentErosionRow VU.! (x - 1)
            geoIndex =
              calculateGeologicIndex
                target
                (x, y)
                upErosion
                leftErosion
            erosionLevel =
              calculateErosionLevel
                depth'
                geoIndex
            risk =
              calculateRisk erosionLevel
            newRiskSum =
              riskSum + risk
        in go
            prevErosionRow
            (currentErosionRow VU.// [(x, erosionLevel)])
            (x + 1, y)
            newRiskSum
