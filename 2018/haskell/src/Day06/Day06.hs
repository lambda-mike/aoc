module Day06.Day06 where

import qualified Data.List as List
import qualified Data.Map.Strict as Map

type BelongingMap = [(Pos, AreaBelonging)]
type Distance = Int
type Pos = (Int, Int) -- X,Y or Col, Row

data AreaBelonging
  = Single Pos
  | Many
  deriving (Eq, Show)

threshold :: Int
threshold = 10000


-- given position on the grid, determine the biggest (yet not infinite) area
solveA :: String -> Int
solveA =
  findBiggestFiniteArea
  . generateBelongingMap
  . parseCoordinates

-- given positions on the grid, determine the biggest area with distance to all given coordinates under given threshold
solveB :: Int -> String -> Int
solveB threshold =
  countLocationsUnderThreshold threshold
  . generateArea
  . parseCoordinates

countLocationsUnderThreshold :: Int -> [(Pos, Distance)] -> Int
countLocationsUnderThreshold threshold =
  List.length
  . List.filter ((< threshold) . snd)

generateArea :: [Pos] -> [(Pos, Distance)]
generateArea coordinates =
  let
    right = findRight coordinates
    down  = findDown coordinates
  in
    List.map
      (\p -> (p, calculateTotalDist coordinates p))
    $ [(col,row) | row <- [0..down],
                   col <- [0..right] ]

calculateTotalDist :: [Pos] -> Pos -> Int
calculateTotalDist coordinates pos =
  List.sum
  $ List.map (manhattanDist pos) coordinates

parseCoordinates :: String -> [Pos]
parseCoordinates =
  map parseLine
  . lines
  where
    parseLine =
      parsePos
      . map (read::String->Int)
      . words
      . (filter (/= ','))
    parsePos [x,y] = (x,y)

generateBelongingMap :: [Pos] -> BelongingMap
generateBelongingMap coordinates =
  let
    right = findRight coordinates
    down  = findDown coordinates
  in
    map (calculateAreaBelonging coordinates) $
    [(col,row) | row <- [0..down],
                 col <- [0..right] ]

calculateAreaBelonging :: [Pos] -> Pos -> (Pos, AreaBelonging)
calculateAreaBelonging coordinates pos@(x,y) =
  let
    sortedCoordinates =
      List.sortOn snd $
      List.map
        (\c -> (c, manhattanDist pos c))
        coordinates
    closestDistance =
      snd
      . head
      $ sortedCoordinates
    closestPoints =
      List.map fst
      . List.takeWhile ((== closestDistance) . snd)
      $ sortedCoordinates
  in
    (pos, recognizeArea closestPoints)

manhattanDist :: Pos -> Pos -> Int
manhattanDist (a1,b1) (a2,b2) =
  List.sum
  $ List.map abs
  $ [a1-a2,b1-b2]

recognizeArea :: [Pos] -> AreaBelonging
recognizeArea [p] = Single p
recognizeArea _   = Many

findRight :: [Pos] -> Int
findRight =
  fst
  . List.head
  . List.reverse
  . List.sortOn fst

findDown :: [Pos] -> Int
findDown =
  snd
  . List.head
  . List.reverse
  . List.sortOn snd

findBiggestFiniteArea :: BelongingMap -> Int
findBiggestFiniteArea belongingMap =
  let
    edgeArea = getEdgeArea belongingMap
    innerArea = getInnerArea belongingMap edgeArea
    areasHistogram = countAreas innerArea -- it will be Map
  in
    getMaxArea areasHistogram

getEdgeArea :: BelongingMap -> [AreaBelonging]
getEdgeArea belongingMap =
  let
    top =
      List.filter ((== 0) . snd . fst) belongingMap
    topRightX =
      fst
      . fst
      . List.head
      . List.reverse
      $ top
    left =
      List.filter
        ((== 0) . fst . fst)
        belongingMap
    bottomLeftY =
      snd
      . fst
      . List.head
      . List.reverse
      $ left
    right =
      List.filter
        ((== topRightX) . fst . fst)
        belongingMap
    down =
      List.filter
        ((== bottomLeftY) . snd . fst)
        belongingMap
    edge =
      top ++ left ++ right ++ down
  in
    List.nub
    $ List.map snd edge


getInnerArea :: BelongingMap -> [AreaBelonging] -> BelongingMap
getInnerArea belongingMap edgeArea =
  List.filter
    (differentThan edgeArea)
    belongingMap

differentThan :: [AreaBelonging] -> (Pos, AreaBelonging) -> Bool
differentThan edgeArea (pos, area) =
  List.all
    (/= area)
    edgeArea

countAreas :: BelongingMap -> Map.Map Pos Int
countAreas =
  List.foldl upsertArea Map.empty

upsertArea :: Map.Map Pos Int -> (Pos, AreaBelonging) -> Map.Map Pos Int
upsertArea histogram (pos, Many) = histogram
upsertArea histogram (pos, Single area) =
  Map.insertWith (+) area 1 histogram

getMaxArea :: Map.Map Pos Int -> Int
getMaxArea =
  Map.foldl max 0

sample = "1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9\n"
