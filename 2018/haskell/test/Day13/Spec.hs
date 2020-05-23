module Day13.Spec where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Test.Hspec

import Day13.Day13

spec :: IO ()
spec = hspec $ do

  beforeAll (readFile "src/Day13/sample.txt") $ do

    describe "Day13" $ do

      describe "solveA" $ do

        it "sample" $ \sample -> do
          solveA sample
            `shouldBe` (7,3)

  beforeAll (readFile "src/Day13/sampleb.txt") $ do

    describe "Day13" $ do

      describe "solveB" $ do

        it "sample" $ \sampleB -> do
          solveB sampleB
            `shouldBe` (6,4)
