module Day10.Day10 where

import Debug.Trace
import qualified Data.Set as Set
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

type Pos = (Int, Int)
type Vel = (Int, Int)
type Star = (Pos, Vel)
type Model = [Star]


solveA :: IO ()
solveA =
  drawStars
  =<< getStarsPicture
  <$> map mirrorY
  <$> (\(_, stars, _) -> stars)
  <$> findSecretMessageMoment 11000
  <$> parseInitialModel
  <$> getInput

solveB :: IO ()
solveB =
  findMainMoment upperBound
  where
    upperBound = 12000

-- for debugging
mainDraw :: IO ()
mainDraw =
  drawStars
  =<< getStarsPicture
  . parseInitialModel
  <$> getInput

parseStar :: String -> Star
parseStar input =
    let part1 = dropUntil '<' input
        sx    = takeUntil ',' part1
        part2 = dropUntil ',' part1
        sy    = takeUntil '>' part2
        part3 = dropUntil '<' part2
        svx   = takeUntil ',' part3
        part4 = dropUntil ',' part3
        svy   = takeUntil '>' part4
        pos   = (read sx , read sy)
        vel   = (read svx, read svy)
    in  (pos, vel)
    where
        dropUntil bracket = tail . dropWhile (/= bracket)
        takeUntil c = takeWhile (/= c)

parseInitialModel :: String -> Model
parseInitialModel =
  map parseStar . lines

moveStar :: Star -> Star
moveStar ((x, y), v@(vx, vy)) =
  ((x + vx, y + vy), v)

updateModel :: Model -> Model
updateModel =
  map moveStar

countDistinctYs :: Model -> Int
countDistinctYs =
  Set.size
  . Set.fromList
  . map (snd . fst)

-- given number of iterations and model, find iteration, for which model has the lowest number of stars with distinct y values
findSecretMessageMoment :: Int -> Model -> (Int, Model, Int)
findSecretMessageMoment n stars =
  findSecretMessageMoment'
    (0, stars, (countDistinctYs stars))
    (0, stars, (countDistinctYs stars))
    [1..n]
  where
    getY (_, _, x) = x

    findSecretMessageMoment' _       moment [] = moment
    findSecretMessageMoment' current lowest (x:xs) =
      let
        next      = goToNextMoment current
        newLowest =
          if getY next < getY lowest
          then next
          else lowest
      in
        findSecretMessageMoment' next newLowest xs

    goToNextMoment (i, m, _) =
      (i+1, newModel, countDistinctYs newModel)
      where
        newModel = updateModel m

mirrorY :: Star -> Star
mirrorY ((x, y), v) =
  ((x, -y), v)

getInput :: IO String
getInput = do
  input <- readFile "src/Day10/input.txt"
  return input

-- print moment in time when all stars align to print message
findMainMoment :: Int -> IO ()
findMainMoment n =
  print
  =<< (\(moment, _, _) -> moment)
  <$> findSecretMessageMoment n
  <$> parseInitialModel
  <$> getInput

-- Drawing
drawStar :: Star -> Picture
drawStar ((x, y), _) =
  Translate
    (fromIntegral x)
    (fromIntegral y)
    $ Color green
    $ circleSolid 1

getStarsPicture :: Model -> Picture
getStarsPicture =
  Pictures . map drawStar

drawStars :: Picture -> IO ()
drawStars =
  display
    (InWindow  "Stars" (1200, 1000) (0, 0))
    (greyN 0.4)

showViewPort ViewPort { viewPortTranslate = vpt, viewPortScale = vps, viewPortRotate = vpr } = "vpt: " ++ show vpt ++ " vps: " ++ show vps

-- simulations
runStars :: Model -> IO ()
runStars model =
  simulate
    (InWindow  "Stars" (1200, 1000) (0, 0))
    (greyN 0.4)
    stepsPerSecond
    model
    getStarsPicture
    (\v _ -> trace ("view: " ++ (showViewPort v)) updateModel)
  where
    stepsPerSecond = 2

sampleSimulation :: IO ()
sampleSimulation =
  runStars $ parseInitialModel sample

sample = "position=< 9,  1> velocity=< 0,  2>\nposition=< 7,  0> velocity=<-1,  0>\nposition=< 3, -2> velocity=<-1,  1>\nposition=< 6, 10> velocity=<-2, -1>\nposition=< 2, -4> velocity=< 2,  2>\nposition=<-6, 10> velocity=< 2, -2>\nposition=< 1,  8> velocity=< 1, -1>\nposition=< 1,  7> velocity=< 1,  0>\nposition=<-3, 11> velocity=< 1, -2>\nposition=< 7,  6> velocity=<-1, -1>\nposition=<-2,  3> velocity=< 1,  0>\nposition=<-4,  3> velocity=< 2,  0>\nposition=<10, -3> velocity=<-1,  1>\nposition=< 5, 11> velocity=< 1, -2>\nposition=< 4,  7> velocity=< 0, -1>\nposition=< 8, -2> velocity=< 0,  1>\nposition=<15,  0> velocity=<-2,  0>\nposition=< 1,  6> velocity=< 1,  0>\nposition=< 8,  9> velocity=< 0, -1>\nposition=< 3,  3> velocity=<-1,  1>\nposition=< 0,  5> velocity=< 0, -1>\nposition=<-2,  2> velocity=< 2,  0>\nposition=< 5, -2> velocity=< 1,  2>\nposition=< 1,  4> velocity=< 2,  1>\nposition=<-2,  7> velocity=< 2, -2>\nposition=< 3,  6> velocity=<-1, -1>\nposition=< 5,  0> velocity=< 1,  0>\nposition=<-6,  0> velocity=< 2,  0>\nposition=< 5,  9> velocity=< 1, -2>\nposition=<14,  7> velocity=<-2,  0>\nposition=<-3,  6> velocity=< 2, -1>"

