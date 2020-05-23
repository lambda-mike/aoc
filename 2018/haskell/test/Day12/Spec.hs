module Day12.Spec where

import Data.Bit (Bit(..))
import qualified Data.Bits as Bits
import qualified Data.Vector.Unboxed as V
import Test.Hspec

import Day12.Day12

spec :: IO ()
spec = hspec $ do

  describe "Day12" $ do

    describe "parsing" $ do

      it "parsePots should return correct generation" $ do

        let gen = "..#.#"
        parsePots gen `shouldBe` 5

      it "parseRule should return correct rule" $ do

        parseRule "..#.# => #" `shouldBe` (5, 1)

      it "parseInput should return correct generation" $ do

        let input =
              "initial state: ###..#.#..\n\n..... => .\n#..## => .\n..### => #\n"
        generation (parseInput input) `shouldBe` 916

      it "parseInput should return correct rules" $ do

        let input = "initial state: ###..#.#..\n\n..... => .\n#..## => .\n..### => #\n"
            parsed = parseInput input
            -- rules: "[0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]"
        (rules parsed V.! 7) `shouldBe` 1
        (V.sum $ rules parsed) `shouldBe` 1

    describe "addPotsToEnd" $ do

      let generateAddPotsToEndTestCase numToAdd = do
            let num = 5
                gen = Bits.shiftL num (4 - numToAdd)
                zeroIndex' = 2 + (4 - numToAdd)
                plantation =
                  initPlantation
                    gen
                    zeroIndex'
                actual =
                  addPotsToEnd plantation

            generation actual
              `shouldBe` Bits.shiftL num 4

            zeroIndexPos actual
              `shouldBe` zeroIndex' + numToAdd

      it "add no missing pots" $ do

        generateAddPotsToEndTestCase 0

      it "add one missing pot" $ do

        generateAddPotsToEndTestCase 1

      it "add two missing pots" $ do

        generateAddPotsToEndTestCase 2

      it "add three missing pots" $ do

        generateAddPotsToEndTestCase 3

      it "add four missing pots" $ do

        generateAddPotsToEndTestCase 4

    describe "removeRedundantPots" $ do

      let expectedPots = 16
          zeroIndex = 4

      it "remove no redundant pots" $ do

        let plantation = initPlantation expectedPots zeroIndex
            actual = removeRedundantPots plantation

        generation actual
          `shouldBe` expectedPots

        zeroIndexPos actual
          `shouldBe` zeroIndex

      it "remove redundant pots" $ do

        let plantation = initPlantation 64 6
            actual = removeRedundantPots plantation

        generation actual
          `shouldBe` expectedPots

        zeroIndexPos actual
          `shouldBe` zeroIndex

    describe "sumPlantsIndexes" $ do

      it "no plants" $ do

        let plantation =
              initPlantation
                0
                0
        sumPlantsIndexes plantation
          `shouldBe` 0

      it "only positive indexs" $ do

        -- "[1,0,1,0,0,0,1,0,1]"
        let plantation =
              initPlantation
                324
                8
        sumPlantsIndexes plantation
          `shouldBe` 8

      it "only negative indexs" $ do

        -- "[1,0,1,0,0,0,1,0,1]"
        let plantation =
              initPlantation
                324
                0
        sumPlantsIndexes plantation
          `shouldBe` (-16)

      it "mixed idnexes" $ do

        -- "[1,0,1,0, 0 ,0,1,0,1,1,0,0,0,1,0,1]"
        let plantation =
              initPlantation
                41669
                11
        sumPlantsIndexes plantation
          `shouldBe` 25

  day12sampleSpec

  where
    initPlantation gen index =
      Plantation
      { generation     = gen
      , rules          = V.empty
      , zeroIndexPos   = index
      }
    day12sampleSpec :: Spec
    day12sampleSpec =

      beforeAll (readFile "src/Day12/sample.txt") $ do

        context "Day12" $ do

          describe "simulate" $ do

              it "sample generation 1" $ \input -> do

                let sample = parseInput input
                    result = simulate 1 sample

                    -- import qualified Data.Bit as B
                    -- cloneToWords (V.drop someNumOfTrailingZeroesAtTheEnd (V.reverse ( read "[0,0,0,1,0,0,0,1,0,0,0,0,1,0,0,0,0,0,1,0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0]" :: V.Vector Bit)))
                (generation result)
                  `shouldBe`
                    71436580

              it "sample generation 10" $ \input -> do

                let sample = parseInput input
                    result = simulate 10 sample

                -- (read "[0,0,0,1,0,1,0,0,1,0,0,0,1,0,1,1,0,0,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,0]")
                (generation result)
                  `shouldBe`
                    2757256806

              it "sample generation 20" $ \input -> do

                let sample = parseInput input
                    result = simulate 20 sample

                -- (read ["0,1,0,0,0,0,1,1,0,0,0,0,1,1,1,1,1,0,0,0,1,1,1,1,1,1,1,0,0,0,0,1,0,1,0,0,1,1,0,0,0,0"]
                (generation result)
                  `shouldBe`
                    144011948198

          describe "solve" $ do

              it "sample" $ \input -> do

                solve 20 input `shouldBe` 325
