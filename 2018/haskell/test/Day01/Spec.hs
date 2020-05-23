module Day01.Spec where

import Test.Hspec

import Day01.Day01 as Day01

spec :: IO ()
spec = hspec $ do

  describe "Day01" $ do

    describe "solveA" $ do

      it "all signs in input" $ do
        let input = "+1\n-2\n+3\n+1"
        solveA input
          `shouldBe` 3

    describe "solveB" $ do

      it "expect 0" $ do
        solveB "+1\n-1"
          `shouldBe` 0

      it  "expect 10" $ do
        solveB "+3\n+3\n+4\n-2\n-4"
          `shouldBe` 10

      it "expect 5" $ do
        solveB "-6\n+3\n+8\n+5\n-6"
          `shouldBe` 5

      it "expect 14" $ do
        solveB "+7\n+7\n-2\n-7\n-4"
          `shouldBe` 14
