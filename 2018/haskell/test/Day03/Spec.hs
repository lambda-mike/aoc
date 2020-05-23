module Day03.Spec where

import Test.Hspec

import Day03.Day03

spec :: IO ()
spec = hspec $ do

  describe "Day03" $ do

    describe "solveA" $ do

      it "returns 0 for single claim" $ do
        solveA claim
          `shouldBe` 0

      it "returns correct result for sample" $ do
        solveA sample
          `shouldBe` 4

    describe "solveB" $ do

      it "returns correct result for sample" $ do
        solveB sample
          `shouldBe` 3
