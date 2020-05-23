module Day05.Spec where

import Test.Hspec

import Day05.Day05

spec :: IO ()
spec = hspec $ do

  describe "Day05" $ do

    describe "solveA" $ do

      it "aA" $ do
        solveA "aA"
          `shouldBe` 0

      it "abBA" $ do
        solveA "abBA"
          `shouldBe` 0

      it "abAB" $ do
        solveA "abAB"
          `shouldBe` 4

      it "aabAAB" $ do
        solveA "aabAAB"
          `shouldBe` 6

      it "sample" $ do
        solveA "dabAcCaCBAcCcaDA"
          `shouldBe` 10

    describe "oppositePolarity " $ do

      it "g G" $ do
        oppositePolarity 'g' 'G'
          `shouldBe` True

      it "G g" $ do
        oppositePolarity 'G' 'g'
          `shouldBe` True

      it "G G" $ do
        oppositePolarity 'G' 'G'
          `shouldBe` False

      it "g g" $ do
        oppositePolarity 'g' 'g'
          `shouldBe` False

      it "g a" $ do
        oppositePolarity 'g' 'a'
          `shouldBe` False

      it "G A" $ do
        oppositePolarity 'G' 'A'
          `shouldBe` False

    describe "solveB" $ do

      it "dabAcCaCBAcCcaDA" $ do
        solveB "dabAcCaCBAcCcaDA"
          `shouldBe` 4

    describe "solveB" $ do

      it "exclude ('a', 'A') from dabAcCaCBAcCcaDA" $ do
        excludeType "dabAcCaCBAcCcaDA" ('a', 'A')
          `shouldBe` "dbcCCBcCcD"

      it "exclude ('b', 'B') from dabAcCaCBAcCcaDA" $ do
        excludeType "dabAcCaCBAcCcaDA" ('b', 'B')
          `shouldBe` "daAcCaCAcCcaDA"

      it "exclude ('c', 'C') from dabAcCaCBAcCcaDA" $ do
        excludeType "dabAcCaCBAcCcaDA" ('c', 'C')
          `shouldBe` "dabAaBAaDA"

      it "exclude ('d', 'D') from dabAcCaCBAcCcaDA" $ do
        excludeType "dabAcCaCBAcCcaDA" ('d', 'D')
          `shouldBe` "abAcCaCBAcCcaA"
