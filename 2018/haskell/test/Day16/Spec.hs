module Day16.Spec where

import Test.Hspec
import Day16.Day16

spec :: IO ()
spec = hspec $ do

  describe "Day16" $ do

    describe "solveA" $ do

      it "sample" $ do
        solveA sample
          `shouldBe` 1

      it "sampleBig" $ do
        solveA sampleBig
          `shouldBe` 2
