module Day18.Day18 where

import Control.Monad (join)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as VB
import qualified Data.Foldable as F
import qualified Data.List as L

-- Ground | Tree | Lumberyard
type Acre = Char

newtype N = N Int -- size of one side (N acres)
  deriving Show

type Area = V.Vector Acre

data Neighbourhood =
  Neighbourhood
    { treesNum :: !Int
    , lumberyardsNum :: !Int
    }

type NeighboursLookupTable = VB.Vector [Int]

ground :: Acre
ground = '.'

tree :: Acre
tree = '|'

lumberyard :: Acre
lumberyard = '#'

iterationsA :: Int
iterationsA = 10

iterationsB :: Int
iterationsB = 1000000000

solve
  :: Int
  -> String
  -> Int
solve iterations =
  calcResourceVal .
  triggerMagic iterations .
  parseArea

solveB
  :: Int
  -> String
  -> Int
solveB time =
  calculateResourceValue time
  . getTreesAndLumberyardsAt time
  . parseArea

getTreesAndLumberyardsAt
  :: Int
  -> (N, Area)
  -> (Int, [(Int, Int)])
getTreesAndLumberyardsAt time (n, area) =
  let statsStream =
        L.map
          (\a ->
            ( countTotal tree a
            , countTotal lumberyard a
            ))
          $ generateListOfAreas
              neighboursLookupTable
              (n, area)
      indexedCycles =
        L.map (\(i, stats) ->
          (i, findCycle cycleWindowSize $ L.take cycleWindowSize $ L.drop (i-1) stats)) $
        L.zip
          [1 .. ]
          (L.repeat statsStream)
  -- find first proper index of max possible length
  in head $ dropWhile ((== []) . snd) indexedCycles
  where
    cycleWindowSize = 150
    neighboursLookupTable =
      generateNeighboursLookupTable n

generateListOfAreas
  :: NeighboursLookupTable
  -> (N, Area)
  -> [Area]
generateListOfAreas neighboursLookupTable state@(n, area) =
  let nextArea =
        V.imap
          (\index _ ->
            executeRulesAcre
              area
              neighboursLookupTable
              n
              index)
          area
  in nextArea : generateListOfAreas neighboursLookupTable (n, nextArea)

findCycle
  :: Int
  -> [(Int, Int)]
  -> [(Int, Int)]
findCycle cycleWindowSize statsWindow =
  join
  . L.take 1
  . L.dropWhile (== [])
  . L.map
      (\(n, stats) ->
        let cycleStream =
              L.take cycleWindowSize $
                L.cycle (L.take n stats)
        in if cycleStream == L.take cycleWindowSize stats
           then L.take n cycleStream
           else []
      )
  $ L.zip
      [1..maxCycleLength]
      statsWindowsStream
  where
    statsWindowsStream =
      L.repeat statsWindow
    maxCycleLength = 50

calculateResourceValue
  :: Int
  -> (Int, [(Int, Int)])
  -> Int
calculateResourceValue iterations (cycleIndex, cycleItems) =
  calcResourceValFromStats $
    cycleItems !!
      ((iterations - cycleIndex)
        `mod` (length cycleItems))
  where
    calcResourceValFromStats
      :: (Int, Int)
      -> Int
    calcResourceValFromStats (treesTotal, lumberyardsTotal) =
      treesTotal * lumberyardsTotal

calcResourceVal :: Area -> Int
calcResourceVal area =
  countTotal tree area * countTotal lumberyard area

countTotal
  :: Acre
  -> Area
  -> Int
countTotal acre =
  V.length . V.filter (== acre)

executeRulesAcre
  :: Area
  -> NeighboursLookupTable
  -> N
  -> Int
  -> Acre
executeRulesAcre
  originalArea
  neighboursLookupTable
  n
  index =
    transformAcre acre $
      getNeighbourhood
        originalArea
        neighboursLookupTable
        n
        index
        acre
  where
    acre = originalArea V.! index

getNeighbourhood
  :: Area
  -> NeighboursLookupTable
  -> N
  -> Int
  -> Acre
  -> Neighbourhood
getNeighbourhood
  originalArea
  neighboursLookupTable
  (N n)
  index
  acre =
    calculateNeighbourhoodLazily
      originalArea
      (neighboursLookupTable VB.! index)
      acre
      zeroNeighbours
  where
    zeroNeighbours =
      Neighbourhood 0 0

calculateNeighbourhoodLazily
  :: Area
  -> [Int]
  -> Acre
  -> Neighbourhood
  -> Neighbourhood
calculateNeighbourhoodLazily
  originalArea
  neighbours
  acre
  neighbourhood
  | neighbours == [] =
      neighbourhood
  | acre == ground     && treesNum'        >= 3                =
      neighbourhood
  | acre == tree       && lumberyardsNum'  >= 3                =
      neighbourhood
  | acre == lumberyard && treesNum' > 0 && lumberyardsNum' > 0 =
      neighbourhood
  | otherwise =
      let index = head neighbours
          newNeighbourhood =
            incrementNeighbour originalArea neighbourhood index
      in calculateNeighbourhoodLazily
           originalArea
           (tail neighbours)
           acre
           newNeighbourhood
  where
    treesNum' = treesNum neighbourhood
    lumberyardsNum' = lumberyardsNum neighbourhood

incrementNeighbour
  :: Area
  -> Neighbourhood
  -> Int
  -> Neighbourhood
incrementNeighbour
  originalArea
  neighbourhood
  index
  | acre == tree =
      neighbourhood { treesNum = treesNum' + 1 }
  | acre == lumberyard =
      neighbourhood { lumberyardsNum = lumberyardsNum' + 1 }
  | otherwise =
      neighbourhood
  where
    acre = originalArea V.! index
    treesNum' = treesNum neighbourhood
    lumberyardsNum' = lumberyardsNum neighbourhood

transformAcre
  :: Acre
  -> Neighbourhood
  -> Acre
transformAcre acre neighborhood
    | acre == ground     && treesNum'        >= 3                    = tree
    | acre == tree       && lumberyardsNum'  >= 3                    = lumberyard
    | acre == lumberyard && (lumberyardsNum' == 0 || treesNum' == 0) = ground
    | otherwise                                                      = acre
    where
      treesNum' = treesNum neighborhood
      lumberyardsNum' = lumberyardsNum neighborhood

triggerMagic
  :: Int
  -> (N, Area)
  -> Area
triggerMagic i state@(n, area)
  | i > 0 =
      let newArea =
            V.imap (\index _ ->
              executeRulesAcre area neighboursLookupTable n index) area
      in triggerMagic (i - 1) (n, newArea)
  | otherwise =
      area
  where
    neighboursLookupTable =
      generateNeighboursLookupTable n

generateNeighboursLookupTable
  :: N
  -> NeighboursLookupTable
generateNeighboursLookupTable (N n) =
  VB.generate
    (n*n)
    (\i ->
      F.foldr'
       (\pos@(y, x) coords ->
          if inArea y && inArea x
          then uncurry getIndex pos : coords
          else coords)
        [] $
      getCoords i)
  where
    getCoords index =
      let row = index `div` n
          col = index `mod` n
      in [ (row-1, col-1), (row-1, col), (row-1, col+1)
         , (row  , col-1),               (row  , col+1)
         , (row+1, col-1), (row+1, col), (row+1, col+1)
         ]
    inArea i = 0 <= i && i < n
    getIndex row col = row * n + col

parseArea
  :: String
  -> (N, Area)
parseArea input =
  (N n, V.fromList (concat rows))
  where
    n = length rows
    rows =
      lines input
