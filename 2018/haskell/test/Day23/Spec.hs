module Day23.Spec where

import Debug.Trace
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Test.Hspec

import Day23.Day23

spec :: IO ()
spec = hspec $ do

  beforeAll (readFile "src/Day23/input.txt") $ do

    describe "Day23" $ do

      describe "parseNanobots" $ do

        it "should return list of proper length for sample" $ \_ -> do
          L.length $ parseNanobots sample
          `shouldBe` 9

        it "should return list of proper length for sampleB" $ \_ -> do
          L.length $ parseNanobots sampleB
          `shouldBe` 6

        it "should return list of proper records for sample" $ \_ -> do
          (parseNanobots sampleB) !! 3
          `shouldBe` (Robot (Pos 14 14 14) 6)

        it "should return list of proper records for sampleB" $ \_ -> do
          parseNanobots sampleB !! 4
          `shouldBe` (Robot (Pos 50 50 50) 200)

      describe "parseNanobot" $ do

        it "should return proper nanobot" $ \_ -> do
          parseNanobot "pos=<0,0,0>, r=4"
          `shouldBe` (Robot (Pos 0 0 0) 4)

        it "should return proper nanobot" $ \_ -> do
          parseNanobot "pos=<1,0,0>, r=1"
          `shouldBe` (Robot (Pos 1 0 0) 1)

        it "should return proper nanobot" $ \_ -> do
          parseNanobot "pos=<4,3,2>, r=3"
          `shouldBe` (Robot (Pos 4 3 2) 3)

      let robotsA = parseNanobots sample
          robotsB = parseNanobots sampleB

      describe "findRobotWithBiggestRadius" $ do

        let input =
              [(Robot (Pos 1 2 3) 2), (Robot (Pos 2 3 4) 0), (Robot (Pos 3 4 5) 7)]

        it "should return proper nanobot" $ \_ -> do
          findRobotWithBiggestRadius input
          `shouldBe` (Robot (Pos 3 4 5) 7)

      describe "findRobotsInRadius" $ do

        it "should return proper nanobot for sample data set" $ \_ -> do
          let input = parseNanobots sample
              robot = (Robot (Pos 0 0 0) 4)
              expected =
                [(Robot (Pos 0 0 0) 4),(Robot (Pos 1 0 0) 1),(Robot (Pos 4 0 0) 3),(Robot (Pos 0 2 0) 1),(Robot (Pos 0 0 3) 1),(Robot (Pos 1 1 1) 1),(Robot (Pos 1 1 2) 1)]
          findRobotsInRadius input robot
            `shouldBe` expected

        it "should return proper nanobot for sampleB data set" $ \_ -> do
          let input = parseNanobots sampleB
              robot = (Robot (Pos 14 14 14) 6)
              expected =
                [(Robot (Pos 12 14 12) 2),(Robot (Pos 16 12 12) 4),(Robot (Pos 14 14 14) 6)]
          findRobotsInRadius input robot
            `shouldBe` expected

      describe "robotInRadius" $ do

        let robot0 = Robot pos0 4
            inRobot0Range = robotInRadius robot0

        it "should return True for the same robot" $ \_ -> do
          Robot (Pos 0 0 0) 4 `shouldSatisfy` inRobot0Range

        it "should return True for robots in range" $ \_ -> do
          Robot (Pos 1 2 0) 4
            `shouldSatisfy` inRobot0Range
          Robot (Pos 1 0 0) 4
            `shouldSatisfy` inRobot0Range
          Robot (Pos 1 2 1) 4
            `shouldSatisfy` inRobot0Range
          Robot (Pos (-1) (-2) (-1)) 4
            `shouldSatisfy` inRobot0Range

        it "should return False for robot not in range" $ \_ -> do
          Robot (Pos 1 2 2) 4
            `shouldNotSatisfy` inRobot0Range
          Robot (Pos (-1) (-2) (-2)) 4
            `shouldNotSatisfy` inRobot0Range

      describe "solveB" $ do

        it "should return proper distance for sampleB" $ \_ -> do
          solveB sampleB
            `shouldBe` 36

      describe "generateOverlappingRobotsLookupTable" $ do

        describe "sampleA" $ do

          let resultA = generateOverlappingRobotsLookupTable robotsA

          it "should return map of proper size" $ \_ -> do
            M.size resultA `shouldBe` L.length robotsA

          it "every robot should be present in its corresponding set" $ \_ -> do
            resultA `shouldSatisfy`
              L.and . M.elems . M.mapWithKey L.elem

          it "every robot should correspond to the set with proper number of nanobots" $ \_ -> do
            M.elems (M.map L.length resultA)
              `shouldBe` [9,2,3,4,4,5,3,4,3]

        describe "sampleB" $ do

          let resultB = generateOverlappingRobotsLookupTable robotsB

          it "should return map of proper size" $ \_ -> do
            M.size resultB `shouldBe` L.length robotsB

          it "every robot should be present in its corresponding set" $ \_ -> do
            resultB `shouldSatisfy`
              L.and . M.elems . M.mapWithKey L.elem

          it "every robot should correspond to the set with proper number of nanobots" $ \_ -> do
            M.elems (M.map L.length resultB)
              `shouldBe` [3,6,5,5,5,6]

          it "robot (10,12,12;2) should overlap with all nanobots" $ \_ -> do
            let robot = Robot (Pos 10 12 12) 2
            resultB M.! robot
                `shouldBe` S.fromList robotsB

          it "robot (10,10,10;5) should overlap with all but one nanobots" $ \_ -> do
            let robot = Robot (Pos 10 10 10) 5
            resultB M.! robot
                `shouldBe`
                  S.fromList
                    [Robot (Pos 10 10 10) 5,Robot (Pos 10 12 12) 2,Robot (Pos 50 50 50) 200]

      describe "countByNumberOfOverlappingRobotsInSet" $ do

        let robots = parseNanobots sampleB
            robotsTable = generateOverlappingRobotsLookupTable robots
            result = countByNumberOfOverlappingRobotsInSet robots robotsTable
            lonelyRobot = Robot (Pos 10 10 10) 5

        it "should return map of counts with proper keys" $ \_ -> do

          M.keys result
            `shouldBe` [2,3,5]

        it "should return map with proper highest count cluster" $ \_ -> do

          M.findMax result
            `shouldBe`
              (5, S.singleton (S.fromList (L.delete lonelyRobot robots)))

      describe "generateNeighbours" $ do

        let point = Pos 10 10 10

        it "should not contain the starting point" $ \_ -> do
          generateNeighboursInRange 1 (Pos 10 10 10)
            `shouldSatisfy` all (/= point)

        it "returns points in range equal 1" $ \_ -> do
          generateNeighboursInRange 1 point
            `shouldSatisfy`
              and . map ((== 1) . distance point)

        it "returns exacply 6 points" $ \_ -> do
          length
            (generateNeighboursInRange 1 (Pos 10 10 10))
            `shouldBe` 6

      describe "calculateSignal" $ do

        it "should return 0 outside range of any nanobot" $ \_ -> do
          calculateSignal robotsB (Pos 300 300 300)
            `shouldBe` 0

        it "should return 1 when in one bot range" $ \_ -> do
          calculateSignal robotsB (Pos 100 100 100)
            `shouldBe` 1

        it "should return 2 when in two bots range" $ \_ -> do
          calculateSignal robotsB (Pos 20 14 14)
            `shouldBe` 2

        it "should return 3 in three bots range" $ \_ -> do
          calculateSignal robotsB (Pos 13 12 12)
            `shouldBe` 3

        it "should return 5 in max signal point" $ \_ -> do
          calculateSignal robotsB (Pos 12 12 12)
            `shouldBe` 5

      describe "findPosClosestToPos0" $ do

          it "Finds proper position" $ \_ -> do
            let maxSignal = 1
                robots =
                  [ Robot (Pos 50 50 50) 100
                  , Robot (Pos 3 3 3) 2
                  , Robot (Pos 5 5 5) 5
                  , Robot (Pos 7 7 7) 7
                  , Robot (Pos 20 20 20) 5
                  ]
                start = Pos 65 100 57
            findPosClosestToPos0
              robots
              maxSignal
              start
                `shouldBe` Pos 0 14 36

      describe "generateRangeTips" $ do

        let robot = Robot (Pos 52 33 23) 57
            tips = generateRangeTips robot

        it "generates proper number of tips" $ \_ -> do
          length tips
            `shouldBe` 6

        it "generates proper tips positions for robot" $ \_ -> do
          tips `shouldBe`
            [ Pos (-5) 33 23
            , Pos 109 33 23
            , Pos 52 (-24) 23
            , Pos 52 90 23
            , Pos 52 33 (-34)
            , Pos 52 33 80
            ]
      describe "findRangeTipWithMaxSignal" $ do

        it "sample" $ \_ -> do
          findRangeTipWithMaxSignal robotsA robotsA
            `shouldBe` (Pos 1 0 0, 3)

        it "sampleB" $ \_ -> do
          findRangeTipWithMaxSignal robotsB (L.take 5 robotsB)
            `shouldBe` (Pos 12 12 12, 5)

        it "sample custom" $ \_ -> do
          let robots =
                [ Robot (Pos 0 0 0) 10
                , Robot (Pos 3 3 3) 8
                , Robot (Pos 2 (-5) 3) 5
                ]
          findRangeTipWithMaxSignal robots robots
            `shouldBe` (Pos 2 0 3, 3)
