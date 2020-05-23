module Day06.Spec where

import Debug.Trace
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Test.Hspec

import Day06.Day06

spec :: IO ()
spec = hspec $ do

  beforeAll (readFile "src/Day06/input.txt") $ do

    describe "Day06" $ do

      describe "solveA" $ do

        it "sample" $ \_ -> do
          solveA sample
            `shouldBe` 17

      describe "solveB" $ do

        it "sample" $ \_ -> do
          let sampleThreshold = 32
          solveB sampleThreshold sample
            `shouldBe` 16

      describe "calculateAreaBelonging" $ do

        it "cab1 - coordinate is the closest to itself" $ \_ -> do
          calculateAreaBelonging [(2,4), (2,5)] (2,4)
          `shouldBe` ((2,4), Single (2,4))

        it "cab2 - choose the closest coordinate" $ \_ -> do
          calculateAreaBelonging [(2,3), (2,8)] (2,4)
          `shouldBe` ((2,4), Single (2,3))

        it "cab3 - set many" $ \_ -> do
          calculateAreaBelonging [(2,3), (2,5)] (2,4)
          `shouldBe` ((2,4), Many)

      describe "find" $ do

        it "findRight" $ \_ -> do
          (findRight . parseCoordinates $ sample)
            `shouldBe` 8

        it "findDown" $ \_ -> do
          (findDown . parseCoordinates $ sample)
            `shouldBe` 9
