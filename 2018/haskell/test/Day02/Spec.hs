module Day02.Spec where

import Test.Hspec

import Day02.Day02

spec :: IO ()
spec = hspec $ do

  describe "Day02" $ do

    describe "countLettersOccurance" $ do

      it "abcdef" $ do
        countLettersOccurance "abcdef"
          `shouldBe` (0, 0)

      it "bababc" $ do
        countLettersOccurance "bababc"
          `shouldBe` (1, 1)

      it "abbcde" $ do
       countLettersOccurance "abbcde"
          `shouldBe` (1, 0)

      it "abcccd" $ do
        countLettersOccurance "abcccd"
          `shouldBe` (0, 1)

      it "aabcdd" $ do
        countLettersOccurance "aabcdd"
          `shouldBe` (1, 0)

      it "abcdee" $ do
        countLettersOccurance "abcdee"
          `shouldBe` (1, 0)

      it "ababab" $ do
        countLettersOccurance "ababab"
          `shouldBe` (0, 1)

      it "nkucgflathzwwijtrevymbdplq" $ do
        countLettersOccurance "nkucgflathzwwijtrevymbdplq"
          `shouldBe` (1, 0)

    describe "solveB" $ do

      it "fghij fguij" $ do
        compareBoxes "fghij" "fguij"
          `shouldBe` Just "fgij"

      it "abcde axcye" $ do
        compareBoxes "abcde" "axcye"
          `shouldBe` Nothing
