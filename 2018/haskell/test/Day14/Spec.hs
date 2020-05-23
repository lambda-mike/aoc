module Day14.Spec where

import Test.Hspec
import Data.Vector.Unboxed as V

import Day14.Day14Core
import Day14.Day14A as Day14A
import Day14.Day14B as Day14B

spec :: IO ()
spec = do
  specA
  specB

specA :: IO ()
specA = hspec $ do
  describe "Day14A" $ do

    describe "solve" $ do

      describe "Given valid input & recipesNum=10" $ do
        it "returns input score for throw away recipes num equal to 0" $ do
          Day14A.solve inputV 10 0 `shouldBe` [3,7,1,0,1,0,1,2,4,5]

        it "returns input score for throw away recipes num equal to 5" $ do
          Day14A.solve inputV 10 5 `shouldBe` [0, 1, 2, 4, 5, 1, 5, 8, 9, 1]

        it "returns input score for throw away recipes num equal to 9" $ do
          Day14A.solve inputV 10 9 `shouldBe` [5, 1, 5, 8, 9, 1, 6, 7, 7, 9]

        it "returns input score for throw away recipes num equal to 18" $ do
          Day14A.solve inputV 10 18 `shouldBe` [9, 2, 5, 1, 0, 7, 1, 0, 8, 5]

        it "returns input score for throw away recipes num equal to 2018" $ do
          Day14A.solve inputV 10 2018 `shouldBe` [5, 9, 4, 1, 4, 2, 9, 8, 8, 2]

specB :: IO ()
specB = hspec $ do
  describe "Day14B" $ do

    describe "solve" $ do

      describe "Given valid input & searched sequence" $ do
        it "returns number of scores for 51589" $ do
          Day14B.solve input "51589" `shouldBe` 9

        it "returns number of scores for 01245" $ do
          Day14B.solve input "01245" `shouldBe` 5

        it "returns number of scores for 92510" $ do
          Day14B.solve input "92510" `shouldBe` 18

        it "returns number of scores for 01245" $ do
          Day14B.solve input "59414" `shouldBe` 2018

inputV :: Vector Score
inputV = inputScores

inputScores :: Vector Score
inputScores = V.fromList input

input = [3, 7]
