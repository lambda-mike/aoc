module Day25.Day25 where

import qualified Data.List as L
import qualified Data.Set as Set
import qualified Data.Vector as V

type Point = (Int,Int,Int,Int)
type Constellation = [Point]
type Constellations = V.Vector Constellation

constellationThreshold :: Int
constellationThreshold = 3

-- get list of fixed points in 4D space, return number of constellations formed
solve :: String -> Int
solve =
  countConstellations
  . discoverConstellations
  . parsePoints

countConstellations :: Constellations -> Int
countConstellations = V.length

discoverConstellations :: [Point] -> Constellations
discoverConstellations points =
  let
    constellations = V.empty
  in
    discoverConstellationsRec
      constellations
      (Set.fromList points)

discoverConstellationsRec
  :: Constellations
  -> Set.Set Point
  -> Constellations
discoverConstellationsRec constellations points =
  if Set.null points then
    constellations
  else
    let (point, restPoints) = Set.deleteFindMin points
        (newConstellation, points') =
            createNewConstellation [] restPoints (Set.singleton point)
        newConstellations = V.cons newConstellation constellations
    in discoverConstellationsRec newConstellations points'

createNewConstellation
  :: [Point]
  -> Set.Set Point
  -> Set.Set Point
  -> ([Point], Set.Set Point)
createNewConstellation constellation points pointsQueue =
  if Set.null pointsQueue then
    (constellation, points)
  else
    let (point, restPointsQueue) = Set.deleteFindMin pointsQueue
        (closePoints, farPoints) =
            Set.partition (closeEnough point) points
        newPoints = farPoints
        newPointsQueue = Set.union restPointsQueue closePoints
    in createNewConstellation (point:constellation) newPoints newPointsQueue

closeEnough :: Point -> Point -> Bool
closeEnough x y =
  manhattanDist x y <= constellationThreshold

manhattanDist :: Point -> Point -> Int
manhattanDist (a1,b1,c1,d1) (a2,b2,c2,d2) =
  sum $
  L.map abs $
  [a1-a2,b1-b2,c1-c2,d1-d2]

parsePoints :: String -> [Point]
parsePoints =
  L.map parsePoint
  . lines
  where
    parsePoint =
      (\[a,b,c,d] -> (a,b,c,d)) .
      L.map (read::String->Int) .
      words .
      L.map (\c -> if c == ',' then ' ' else c)

sample1 = "0,0,0,0\n3,0,0,0\n0,3,0,0\n0,0,3,0\n0,0,0,3\n0,0,0,6\n9,0,0,0\n12,0,0,0\n"

sample2 = "-1,2,2,0\n0,0,2,-2\n0,0,0,-2\n-1,2,0,0\n-2,-2,-2,2\n3,0,2,-1\n-1,3,2,2\n-1,0,-1,0\n0,2,1,-2\n3,0,0,0\n"

sample3 = "1,-1,0,1\n2,0,-1,0\n3,2,-1,0\n0,0,3,1\n0,0,-1,-1\n2,3,-2,0\n-2,2,0,0\n2,-2,0,-1\n1,-1,0,-1\n3,2,0,2\n"

sample4 = "1,-1,-1,-2\n-2,-2,0,1\n0,2,1,3\n-2,3,-2,1\n0,2,3,-2\n-1,-1,1,-2\n0,-2,-1,0\n-2,2,3,-1\n1,2,2,0\n-1,-2,0,-2\n"
