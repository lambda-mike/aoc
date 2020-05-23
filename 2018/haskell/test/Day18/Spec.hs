module Day18.Spec where

import qualified Data.List as L
import qualified Data.Vector as V
import Test.Hspec

import Day18.Day18

spec :: IO ()
spec = hspec $ do

  beforeAll (readFile "src/Day18/input.txt") $ do

    describe "Day18" $ do

      describe "parsing" $ do

        it "parseArea should return correct area" $ \_ -> do

          let expected = ".||##.....\n||###.....\n||##......\n|##.....##\n|##.....##\n|##....##|\n||##.####|\n||#####|||\n||||#|||||\n||||||||||"
              sample = ".#.#...|#.\n.....#|##|\n.|..|...#.\n..|#.....#\n#.#|||#|#|\n...#.||...\n.|....|...\n||...#|.#|\n|.||||..|.\n...#.|..|."

          snd (parseArea expected) `shouldBe`
            (triggerMagic iterationsA $ -- from Day18
              parseArea sample)

      describe "solve" $ do

        it "sample" $ \input -> do

          solve iterationsA input
            `shouldBe` 495236

      context "findCycle" $ do

        let size = 150

        describe "Returns the proper cycle" $ do

          it "of size 1" $ \_ -> do

            findCycleSuccessTest size [(111, 222)]

          it "of size 2" $ \_ -> do

            findCycleSuccessTest size [(111, 222), (222, 333)]

          it "of size 3" $ \_ -> do

            findCycleSuccessTest size [(111, 222), (222, 333), (333, 444)]

          it "of size 4" $ \_ -> do

            findCycleSuccessTest size [(111, 222), (222, 333), (333, 444), (444, 555)]

        describe "Returns the proper cycle" $ do

          it "of size 1" $ \_ -> do

            findCycleFailureTest size [(111, 222)]

          it "of size 2" $ \_ -> do

            findCycleFailureTest size [(111, 222), (222, 333)]

          it "of size 3" $ \_ -> do

            findCycleFailureTest size [(111, 222), (222, 333), (333, 444)]

          it "of size 4" $ \_ -> do

            findCycleFailureTest size [(111, 222), (222, 333), (333, 444), (444, 555)]

findCycleSuccessTest size stats = do

  let arr = L.take size $ L.cycle stats

  findCycle size arr
    `shouldBe` stats

findCycleFailureTest size stats = do
  let arr = L.take size $ L.cycle stats

  findCycle size (L.init arr ++ [(12,34)])
    `shouldBe` []

