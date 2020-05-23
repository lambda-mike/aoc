module Day11.Day11A where
-- TODO
-- Criterion for different windows generation/traversal fns??

import qualified Data.Map as Map

type Pos = (Int, Int)
type Power = Int
type SerialNumber = Int
type Cell = (Pos, Power)

serialNumber :: Int
serialNumber = 7400

width :: Int
width = 300

height :: Int
height = 300

-- TODO 1 change to squareSize
windowSize :: (Int, Int)
windowSize = (3,3)

-- TODO 2 change to Map (x, y, size) Power and return value as well
-- TODO 3 change windowSize to squareSizeLimit
-- TODO 4 traverse L -> R T -> B square size 2, 3, 4, ...
findMostPowerfullCellWindow :: Int -> Int -> (Int, Int) -> [Cell] -> Pos
findMostPowerfullCellWindow width height windowSize cells =
  let
    powerGrid = Map.fromList cells
    zeroWin = ((0,0), minBound :: Int)
    windowsCorners =
      generateGrid (width - fst windowSize + 1) (height - snd windowSize + 1)
  in
    fst $ traverseWindows powerGrid windowSize windowsCorners zeroWin


traverseWindows :: Map.Map Pos Int
  -> (Int, Int)
  -> [Pos]
  -> (Pos, Int)
  -> (Pos, Int)
traverseWindows _     windowSize [ ]            win = win
traverseWindows powerGrid windowSize (winCorner:otherCorners) win =
  let
    winPower =
      sum $
      populatePower powerGrid $
      generateWindow windowSize winCorner
  in
    if winPower > snd win then
      traverseWindows powerGrid windowSize otherCorners (winCorner, winPower)
    else
      traverseWindows powerGrid windowSize otherCorners win

populatePower :: Map.Map Pos Int -> [Pos] -> [Int]
populatePower powerGrid =
  map (powerGrid Map.!)

-- TODO 5 when generating window, reuse grid cached values
generateWindow :: (Int, Int) -> Pos -> [Pos]
generateWindow (wx,wy) (x,y) =
  [ (nx, ny) | ny <- [y..limitY], nx <- [x..limitX] ]
  where
    limit a da = a + da - 1
    limitY = limit y wy
    limitX = limit x wx

calculateCellsPower :: SerialNumber -> [Pos] -> [Cell]
calculateCellsPower sn =
  map (calculateCellPower sn)

calculateCellPower :: SerialNumber -> Pos -> Cell
calculateCellPower sn pos@(x,y) =
  let power = (keepHundreds $ (rackId * y + sn) * rackId) - 5
  in  (pos, power)
  where
    rackId = x + 10

keepHundreds :: Int -> Int
keepHundreds n
  | n < 100 = 0
  | otherwise =
    n `div` 100 `rem` 10

generateGrid :: Int -> Int -> [Pos]
generateGrid w h =
  [ (x, y) | y <- [1..h], x <- [1..w] ]

solve :: Int -> Int -> SerialNumber -> (Int, Int) -> Pos
solve w h sn windowSize =
  findMostPowerfullCellWindow w h windowSize $
  calculateCellsPower sn $
  generateGrid w h

