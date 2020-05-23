module Day08.Spec where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Test.Hspec

import Day08.Day08

spec :: IO ()
spec = hspec $ do

  beforeAll (readFile "src/Day08/input.txt") $ do

    describe "Day08" $ do

      describe "solveA" $ do

        it "(empty)" $ \_ -> do
          solveA ""
            `shouldBe` 0

        it "0 3 10 11 12" $ \_ -> do
          solveA "0 3 10 11 12"
            `shouldBe` 33

        it "1 1 0 1 99 2" $ \_ -> do
          solveA "1 1 0 1 99 2"
            `shouldBe` 101

        it "sample" $ \_ -> do
          solveA sample
            `shouldBe` 138

      describe "solveB" $ do

        it "0 3 10 11 12" $ \_ -> do
          solveB "0 3 10 11 12"
            `shouldBe` 33

        it "1 1 0 1 99 2" $ \_ -> do
          solveB "1 1 0 1 99 2"
            `shouldBe` 0

        it "sample" $ \_ -> do
          solveB sample
            `shouldBe` 66
