module Day17.Day17
  ( Dir(..)
  , solve
  , solveB
  , parseClayCoords
  , d17oneCellWidth
  , d17oneReservoir
  , d17reservoirWithBox
  , d17sample
  ) where

import Data.Function ((&), on)
import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Prelude hiding (Left, Right)
import Control.Monad ((>>))

type Pos = (Int, Int) -- x, y

data Dir
  = Down
  | Left
  | Right
  deriving (Eq, Show)

data Cell
  = Sand
  | Clay
  | WaterStill
  | WaterSpring
  deriving (Eq, Show)

data SimData =
  SimData
    { caveMap :: Map Pos Cell
    , liveWater :: [(Pos, Dir)]
    , minY :: Int -- position of the closest clay cell in Y axis
    , maxY :: Int -- position of the farest clay cell in Y axis
    }
  deriving Show

solveB
  :: String
  -> [(Pos, Dir)]
  -> Int
solveB input springs
  = countStillWaterCells
  . runSimulation
  . (flip createSimulationData springs)
  . parseClayCoords
  $ input

solve
  :: String
  -> [(Pos, Dir)]
  -> Int
solve input springs
  = countWaterCells
  . runSimulation
  . (flip createSimulationData springs)
  . parseClayCoords
  $ input

countStillWaterCells
  :: SimData
  -> Int
countStillWaterCells =
  countCells [WaterStill]

countWaterCells
  :: SimData
  -> Int
countWaterCells =
  countCells [WaterStill, WaterSpring]

countCells
  :: [Cell]
  -> SimData
  -> Int
countCells cells simData
  = M.size
  . M.filterWithKey
      (\(_, y) _ ->
        minY simData <= y && y <= maxY simData)
  . M.filter
      (\cell ->
        any (== cell) demandedTypesOfCells)
  . caveMap
  $ simData
  where
    demandedTypesOfCells =
      L.nub cells

runSimulation
  :: SimData
  -> SimData
runSimulation sim
  | shouldStop sim =
    sim
  | otherwise =
    runSimulation
      (advanceWater sim)

shouldStop sd =
  [] == (liveWater sd)

advanceWater
  :: SimData
  -> SimData
advanceWater simData =
  L.foldl'
    advanceWaterCell
    simData
    (liveWater simData)

advanceWaterCell
  :: SimData
  -> (Pos, Dir)
  -> SimData
advanceWaterCell simDataX lwc@(pos, dir)
  -- do nothing when live cell does not exist (was removed during previous turn)
  | all (/= lwc) (liveWater simDataX) =
    simDataX
  | Down == dir =
    case nextCell caveMap' pos dir of
      Sand ->
        move dir simData pos
      Clay ->
        fillOrSplit simData pos
      WaterSpring ->
        killOneLiveWaterCell simData pos dir
      WaterStill ->
        fillOrSplit simData pos
  | otherwise =
    case nextCell caveMap' pos dir of
      Sand ->
        fillOr (move dir) simData pos
      Clay ->
        killOneLiveWaterCell simData pos dir
      WaterSpring ->
        killOneLiveWaterCell simData pos dir
      WaterStill ->
        error
          $ "Water should never reach still water moving horizontally!"
          <> show pos
          -- because when transformed to still all live water should be cleaned
  where
    caveMap' = caveMap simData
    sortedLiveWater = sortLiveWater simDataX
    simData = sortedLiveWater
    sortLiveWater simData' =
      let liveWater' = liveWater simData'
          sorted =
            L.sortBy
              (\((x1, y1), _) ((x2, y2), _) ->
                compare (-y1, x1) (-y2, x2))
              liveWater'
      in simData' { liveWater = sorted }

nextCell
  :: Map Pos Cell
  -> Pos
  -> Dir
  -> Cell
nextCell caveMap' (x, y) dir =
  case dir of
    Down ->
      (x, y + 1)
    Left ->
      (x - 1, y)
    Right ->
      (x + 1, y)
  & lookupCell caveMap'

lookupCell
  :: Map Pos Cell
  -> Pos
  -> Cell
lookupCell caveMap' pos =
  caveMap' M.!? pos
  & maybe Sand id

move
  :: Dir
  -> SimData
  -> Pos
  -> SimData
move dir simData pos@(x, y) =
  case dir of
    Down ->
      let newPos =
            (x, y + 1)
      in if y < maxY simData
         then (updateLiveWater simData (pos, dir) (newPos, Down))
                { caveMap =
                    M.insert
                      newPos
                      WaterSpring -- only spring water can move
                      (caveMap simData)
                }
         else killOneLiveWaterCell simData pos dir

    Left ->
      moveToTheSide simData pos dir
    Right ->
      moveToTheSide simData pos dir

moveToTheSide
  :: SimData
  -> Pos
  -> Dir
  -> SimData
moveToTheSide simData pos@(x, y) dir =
  if Down == dir then
    error "how move to the side when dir is Down?!"
  else
    let newPos =
          movePos pos dir
        cellBelowNewPos =
          nextCell
            (caveMap simData)
            newPos
            Down
        simData' =
          simData
            { caveMap =
                M.insert
                  newPos
                  WaterSpring -- only spring water can move
                  (caveMap simData)
            }
    in case cellBelowNewPos of
         Sand ->
           updateLiveWater simData' (pos, dir) (newPos, Down)
         WaterSpring ->
           killOneLiveWaterCell simData' pos dir
         _ ->
           updateLiveWater simData' (pos, dir) (newPos, dir)

movePos
  :: Pos
  -> Dir
  -> Pos
movePos (x, y) Left  = (x - 1, y)
movePos (x, y) Right = (x + 1, y)
movePos (x, y) Down  = (x, y + 1)

updateLiveWater
  :: SimData
  -> (Pos, Dir)
  -> (Pos, Dir)
  -> SimData
updateLiveWater simData (oldPos, oldDir) newLiveWaterCell =
  simData
    { liveWater =
        newLiveWaterCell :
          (liveWater $
            killOneLiveWaterCell simData oldPos oldDir)
    }

killOneLiveWaterCell
  :: SimData
  -> Pos
  -> Dir
  -> SimData
killOneLiveWaterCell
  simData
  pos
  dir
  = simData
    { liveWater =
        excludeOneCell
          pos
          dir
          (liveWater simData)
    }
  where
    excludeOneCell pos dir
      = mergeCells
      . tailSnd
      . L.span (/= (pos, dir))
    tailSnd (xs, ys) =
      let tailYs =
            if [] == ys
            then []
            else L.tail ys
      in (xs, tailYs)
    mergeCells = uncurry (++)

fillOrSplit
  :: SimData
  -> Pos
  -> SimData
fillOrSplit simData pos@(x, y) =
  maybe
    (split simData pos)
    (\range -> fill simData pos range)
    (tryGetReservoirEdges simData pos)

fillOr
  :: (SimData -> Pos -> SimData)
  -> SimData
  -> Pos
  -> SimData
fillOr op simData pos@(x, y) =
  maybe
    (op simData pos)
    (\range -> fill simData pos range)
    (tryGetReservoirEdges simData pos)

tryGetReservoirEdges
  :: SimData
  -> Pos
  -> Maybe (Pos, Pos) -- left and right edge
tryGetReservoirEdges simData pos =
  let caveMap' = caveMap simData
  in (,)
    <$> findReservoirEdge caveMap' pos Left
    <*> findReservoirEdge caveMap' pos Right

findReservoirEdge
  :: Map Pos Cell
  -> Pos
  -> Dir
  -> Maybe Pos
findReservoirEdge caveMap' pos dir =
  case nextCell caveMap' pos Down of
    Sand -> Nothing
    WaterSpring -> Nothing
    -- we have ground below our feet -> proceed to the side
    _ -> case nextCell caveMap' pos dir of
           Clay ->
             Just $ movePos pos dir
           WaterSpring ->
             -- ignore existing water springs - we need to carry on
             findReservoirEdge
               caveMap'
               (movePos pos dir)
               dir
           WaterStill ->
            -- If this happens it means the previous live water cell
            -- has already filled in the row
            -- this cell is dead, but the fold has to finish
             Just $ movePos pos dir
           Sand ->
             findReservoirEdge
               caveMap'
               (movePos pos dir)
               dir

fill
  :: SimData
  -> Pos
  -> (Pos, Pos)
  -> SimData
fill simData pos@(x, y) ((xfrom, _), (xto, _)) =
  simData
    { caveMap =
        L.foldl'
          (\caveMap' xn ->
            M.insert (xn, y) WaterStill caveMap')
          (caveMap simData)
          xrange
    , liveWater =
       -- 1. kill all live water cells found in the filled in row
       -- 2. make all WaterSpring cells row above still row live water
        liveWaterAboveStillRow
        ++
        (filter
          (\(lwPos, _) ->
            all (/= lwPos) $ zip xrange (repeat y))
          (liveWater simData))
    }
  where
    xmin = min xfrom xto
    xmax = max xfrom xto
    xrange =
      [ xmin + 1 .. xmax - 1 ] -- inner space without walls
    liveWaterAboveStillRow =
      (flip L.zip (repeat Down)) $
      M.keys $
      M.filterWithKey
        (\(lwx, lwy) lw ->
          lwy == (y-1)
          && any (== lwx) xrange
          && lw == WaterSpring)
        (caveMap simData)

split
  :: SimData
  -> Pos
  -> SimData
split simData pos =
  let simData' =
        updateLiveWater
          simData
          (pos, Down)
          (pos, Right)
  in simData'
     { liveWater =
       (pos, Left) :
        (liveWater simData')
     }

createSimulationData
  :: [Pos]
  -> [(Pos, Dir)]
  -> SimData
createSimulationData pos springs =
  SimData
    { caveMap =
        M.fromList $
          zip (map fst springs) (repeat WaterSpring) ++
          zip pos (repeat Clay)
    , liveWater = springs
    , minY = L.foldl' min (maxBound :: Int)
              $ map snd pos
    , maxY = L.foldl' max 0 $ map snd pos
    }

--"x=495, y=2..7\ny=7, x=495..501\n ..."
parseClayCoords
  :: String
  -> [Pos]
parseClayCoords =
  concatMap (parseLine . words)
  . lines
  where
    parseLine
      :: [String] -- words
      -> [Pos]
    parseLine [part1, part2] =
      let part1Nums = tail . tail . init $ part1 -- ignore x= and ,
          part2Nums = tail . tail $ part2        -- ignore y=
          xstr = if any (== 'x') part1 then part1Nums else part2Nums
          ystr = if any (== 'y') part1 then part1Nums else part2Nums
          x = parseCoord xstr
          y = parseCoord ystr
          len = (max `on` length) x y
          xs = take len $ cycle x
          ys = take len $ cycle y
      in zip xs ys

    parseCoord str =
      if containsDots str
      then parseRange str
      else [ read str :: Int ]

    parseRange r =
      let start = takeWhile (/= '.') r
          end = drop (length start + 2) r -- 2 dots
      in [read start :: Int .. read end :: Int]

    containsDots =
      any (== '.')

d17sample =
  "x=495, y=2..7\ny=7, x=495..501\nx=501, y=3..7\nx=498, y=2..4\nx=506, y=1..2\nx=498, y=10..13\nx=504, y=10..13\ny=13, x=498..504"

d17oneCellWidth = "x=490, y=1\nx=499, y=3..5\nx=501, y=3..5\nx=500, y=5\n"

d17oneReservoir = "x=490, y=1\nx=495..505, y=5\nx=495, y=3..4\nx=505, y=3..4\n"

d17reservoirWithBox = "x=490, y=1\nx=495..505, y=6\nx=495, y=3..5\nx=505, y=3..5\nx=500, y=4..5"

-- DEBUG
solveDebug
  :: String
  -> [(Pos, Dir)]
  -> IO Int
solveDebug input springs = do
  let clayCoords = parseClayCoords input
      simData = createSimulationData clayCoords springs
      prevSims = [simData]
  go prevSims
  where
    go prevSims = do
      let prevSim = head prevSims
      if shouldStop prevSim
      then do
        -- mapM_ printWaterSurroundings prevSims
        print prevSim
        return $ countWaterCells prevSim
      else do
        let nextSim = advanceWater prevSim
        putStrLn $
          show $
          M.size $
          M.filter (== WaterSpring) $
          (caveMap nextSim)
        printWaterSurroundings nextSim
        go (take 3 $ nextSim : prevSims)


printWaterSurroundings
  :: SimData
  -> IO SimData
printWaterSurroundings simData = do
  let liveWater' = liveWater simData
      size = 10
      liveWaterSurroundings =
        L.map (\x -> [show (fst x) <> "\n" ] <> showCellSurroundings size x) liveWater'
  mapM_ (\x -> (mapM_ putStrLn x) >> putStrLn "") liveWaterSurroundings
  return simData
  where
    showCellSurroundings edge (pos@(x, y), dir) =
      [ (x, y) | y <- [-edge..edge], x <- [-edge..edge] ]
      & L.map (\(dx,dy) -> (x + dx, y + dy))
      & L.zip [0..]
      & L.map (\(i, c) -> (i `div` (length $ [-edge..edge]), c))
      & L.groupBy ((==) `on` fst)
      & L.map (L.map snd)
      & L.map
          (L.concatMap
            (\c ->
              if pos == c
              then take 1 $ show dir
              else showCell $ lookupCell (caveMap simData) c))

showCell :: Cell -> String
showCell Clay = "#"
showCell Sand = "."
showCell WaterSpring = "|"
showCell WaterStill = "~"
