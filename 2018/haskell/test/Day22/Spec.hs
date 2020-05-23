module Day22.Spec where

import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Test.Hspec

import Day22.Day22

spec :: IO ()
spec = hspec $ do

  describe "Day22" $ do

    let sampleInput =
          "depth: 510\ntarget: 10,10"

    describe "solveA" $ do

      it "sample" $
        solveA sampleInput
          `shouldBe` 114

      it "input" $
        solveA input
          `shouldBe` 7299

    describe "solveB" $ do

      it "sample" $
        solveB sampleInput
          `shouldBe` 45

    describe "parseInput" $ do

      it "should return puzzle data for sample" $ do

        parseInput sampleInput `shouldBe`
          (510, (10,10))

      it "should return puzzle data for input" $ do

        parseInput input `shouldBe`
          (11109, (9,731))

    describe "calculateGeologicIndex" $ do

      let target = (10,10)

      it "in 0,0" $ do

        calculateGeologicIndex target (0, 0) undefined undefined
          `shouldBe` 0

      it "in 1,0" $ do

        calculateGeologicIndex target (1, 0) undefined undefined
          `shouldBe` geoX

      it "in 0,1" $ do

        calculateGeologicIndex target (0, 1) undefined undefined
          `shouldBe` geoY

      it "in 1,1" $ do

        let erosionUp = 8415
            erosionLeft = 17317
        calculateGeologicIndex target (1, 1) erosionUp erosionLeft
          `shouldBe` 145722555

      it "in target" $ do

        calculateGeologicIndex
          target
          ((fst target), (snd target))
          undefined
          undefined
            `shouldBe` 0

    describe "calculateErosionLevel" $ do

      it "geoIndex 0" $ do

        calculateErosionLevel 510 0
          `shouldBe` 510

      it "geoIndex geoX" $ do

        calculateErosionLevel 510 geoX
          `shouldBe` 17317

      it "geoIndex geoY" $ do

        calculateErosionLevel 510 geoY
          `shouldBe` 8415

      it "geoIndex 145722555" $ do

        calculateErosionLevel 510 145722555
          `shouldBe`  1805

    describe "calculateRisk" $ do

      it "510" $
        calculateRisk 510 `shouldBe` 0
      it "17317" $
        calculateRisk 17317 `shouldBe` 1
      it "1805" $
        calculateRisk 1805 `shouldBe` 2

    describe "cavePopulateNextLevel" $ do

      let depth = 510
          target = (10,10)
          cave = initializeCave (depth, target)

      it "populates 10th row of cave correctly" $

        VU.slice (depth * 10) 11
          (erosionData (iterate reveal cave !! 10))
            `shouldBe`
              VU.fromList
                [19011,1354,1537,19570,6971,17785,17120,17698,14611,4501,510]
