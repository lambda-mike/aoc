module Day25.Spec where

import Test.Hspec
import Day25.Day25

spec :: IO ()
spec = hspec $ do

  describe "Day25" $ do

    describe "solve" $ do

      it "sample1" $ do
        solve sample1
          `shouldBe` 2

      it "sample2" $ do
        solve sample2
          `shouldBe` 4

      it "sample3" $ do
        solve sample3
          `shouldBe` 3

      it "sample4" $ do
        solve sample4
          `shouldBe` 8

    describe "manhattanDist" $ do

      it "manhattanDist1" $ do
        manhattanDist (1,2,3,4) (1,2,3,4)
          `shouldBe` 0

      it "manhattanDist1" $ do
        manhattanDist (2,2,3,4) (1,2,3,4)
          `shouldBe` 1

      it "manhattanDist1" $ do
        manhattanDist (2,2,3,4) (1,2,3,6)
          `shouldBe` 3

      it "manhattanDist1" $ do
        manhattanDist (2,5,5,4) (1,2,3,6)
          `shouldBe` 8

