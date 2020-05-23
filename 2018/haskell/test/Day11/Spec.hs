module Day11.Spec where

import qualified Data.Vector.Unboxed as V
import Test.Hspec

import Day11.Day11

spec :: IO ()
spec = hspec $ do

  describe "Day11" $ do

    describe "keepHundreds" $ do

      it "0" $
        keepHundreds 0 `shouldBe` 0

      it "30" $
        keepHundreds 30 `shouldBe` 0

      it "100" $
        keepHundreds 100 `shouldBe` 1

      it "500" $
        keepHundreds 500 `shouldBe` 5

      it "123987398" $
        keepHundreds 123987398 `shouldBe` 3

    describe "calculateCellPower" $ do

      it "8 (3,5)" $
        calculateCellPower 8 (3,5)
          `shouldBe` 4

      it "57 (122,79)" $
        calculateCellPower 57 (122,79)
          `shouldBe` (-5)

      it "39 (217,196)" $
        calculateCellPower 39 (217,196)
          `shouldBe` 0

      it "71 (101,153)" $
        calculateCellPower 71 (101,153)
          `shouldBe` 4

    describe "generateSquareCells" $ do

      it "generates one cell for square size 1" $ do

        generateSquareCells 1 3 5 `shouldBe` [ (3, 5, 1) ]

      it "generates four cells for square size 2" $ do

        generateSquareCells 2 6 8 `shouldBe`
          [ (6, 8, 1)
          , (6, 9, 1)
          , (7, 8, 1)
          , (7, 9, 1)
          ]

      it "generates six cells for square size 3" $ do

        generateSquareCells 3 4 5 `shouldBe`
          [ (4, 5, 2)
          , (4, 7, 1)
          , (5, 7, 1)
          , (6, 5, 1)
          , (6, 6, 1)
          , (6, 7, 1)
          ]

      it "generates sixteen cells for square size 8" $ do

        length (generateSquareCells 8 6 8)
          `shouldBe` 4

    describe "solve" $ do

      it "serial number 18" $ do

        let sn = 18

        solve width height sn (Just squareSize) `shouldBe` ((33,45,3), 29)

      it "serial number 42" $ do

        let sn = 42

        solve width height sn (Just squareSize) `shouldBe` ((21,61,3), 30)
