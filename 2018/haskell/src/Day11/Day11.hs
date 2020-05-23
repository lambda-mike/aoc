module Day11.Day11 where
-- TODO
-- Criterion for different windows generation/traversal fns??

import Data.Foldable (foldr')
import Data.Map (Map)
import qualified Data.Map as M

type X = Int
type Y = Int
type Pos = (X, Y)
type Power = Int
type SerialNumber = Int
type Size = Int
type Square = (X, Y, Size)
type Cell = (Pos, Power)

serialNumber :: Int
serialNumber = 7400

width :: Int
width = 300

height :: Int
height = 300

squareSize :: Int
squareSize = 3

findMostPowerfullCellWindow
  :: Int
  -> Int
  -> Maybe Int
  -> ([(Square, Power)], (Square, Power))
  -> (Square, Power)
findMostPowerfullCellWindow width height mSquareSize (squares, maxSquare) =
  let
    powerGrid = M.fromList squares
    row = 1
    col = 1
    square = 2
  in if (Just 1) == mSquareSize
     then maxSquare
     else traverseSquares mSquareSize maxSquare powerGrid square row col

traverseSquares
  :: Maybe Size
  -> (Square, Power)
  -> Map Square Power
  -> Size
  -> Y
  -> X
  -> (Square, Power)
traverseSquares mSquareSize maxSquare powerGrid square row col
  | mSquareSize /= Nothing && Just square > mSquareSize =
      maxSquare
  | mSquareSize == Nothing && square > width =
      maxSquare
  | (col + square - 1) > width =
      traverseSquares
        mSquareSize
        maxSquare
        powerGrid
        square
        (row + 1)
        1
  | (row + square - 1) > height =
      traverseSquares
        mSquareSize
        maxSquare
        powerGrid
        (square + 1)
        1
        1
  | otherwise =
      let cells = generateSquareCells square col row
          squarePower = sum $ populatePower powerGrid cells
          (winSquare, winPower) = maxSquare
          newPowerGrid =
            M.insert
              (col, row, square)
              squarePower
              powerGrid
          newMaxSquare =
            if squarePower >= winPower
            then ((col, row, square), squarePower)
            else maxSquare
      in traverseSquares
          mSquareSize
          newMaxSquare
          newPowerGrid
          square
          row
          (col + 1)

generateSquareCells
  :: Size
  -> X
  -> Y
  -> [Square]
generateSquareCells square col row
  | square == 1 =
      [(col, row, 1)]
  | square `mod` 2 == 0 =
      let smallSquareSize = square `div` 2
      in [ (col, row, smallSquareSize)
         , (col, row + smallSquareSize, smallSquareSize)
         , (col + smallSquareSize, row, smallSquareSize)
         , (col + smallSquareSize, row + smallSquareSize, smallSquareSize)
         ]
  | otherwise =
      (col, row, (square - 1)) :
        (init [ (x, y, 1)
              | y <- [ row + square - 1 ]
              , x <- [ col .. col + square - 1 ]
              ]
        )
        ++
        [ (x, y, 1)
        | y <- [ row .. row + square - 1 ]
        , x <- [ col + square - 1 ]
        ]

populatePower
  :: Map Square Power
  -> [Square]
  -> [Power]
populatePower powerGrid =
  map (powerGrid M.!)

generateWindow
  :: Int
  -> Pos
  -> [Pos]
generateWindow squareSize (x,y) =
  [ (nx, ny) | ny <- [y..limitY], nx <- [x..limitX] ]
  where
    limit a da = a + da - 1
    limitY = limit y squareSize
    limitX = limit x squareSize

calculateCellsPower
  :: SerialNumber
  -> [Pos]
  -> ([(Square, Power)], (Square, Power))
calculateCellsPower sn =
  foldr'
    (\pos@(x, y) (squares, maxSquareSoFar@(_, maxPower)) ->
      let power = calculateCellPower sn pos
          square = (x, y, squareSize)
          squareWithPower = (square, power)
          newMaxSquare =
            if power >= maxPower
            then squareWithPower
            else maxSquareSoFar
      in (squareWithPower : squares, newMaxSquare)
    )
    ([], ultimateMin)
  where
    squareSize = 1
    ultimateMin = ((0,0,0), minBound::Int)


calculateCellPower :: SerialNumber -> Pos -> Power
calculateCellPower sn pos@(x,y) =
  (keepHundreds $ (rackId * y + sn) * rackId) - 5
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

solve
  :: Int
  -> Int
  -> SerialNumber
  -> Maybe Int
  -> (Square, Power)
solve w h sn squareSize =
  findMostPowerfullCellWindow w h squareSize $
  calculateCellsPower sn $
  generateGrid w h

solveA
  :: Int
  -> Int
  -> SerialNumber
  -> (Square, Power)
solveA w h sn =
  solve w h sn (Just 3)

solveB
  :: Int
  -> Int
  -> SerialNumber
  -> (Square, Power)
solveB w h sn =
  solve w h sn Nothing
