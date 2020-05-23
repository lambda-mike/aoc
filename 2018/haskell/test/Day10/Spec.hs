module Day10.Spec where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Test.Hspec

import Day10.Day10

spec :: IO ()
spec = hspec $ do

  beforeAll (readFile "src/Day10/input.txt") $ do

    describe "Day10" $ do

      describe "parseStar" $ do

        it "s1" $ \_ -> do
          parseStar
            "position=< 9,  1> velocity=< 0,  2>"
              `shouldBe` ((9,1), (0,2))

        it "s2" $ \_ -> do
          parseStar
            "position=< 3, -2> velocity=<-1,  1>"
              `shouldBe` ((3, -2), (-1,1))

        it "s3" $ \_ -> do
          parseStar "position=<-3,  2> velocity=<-1, -1>"
              `shouldBe` ((-3, 2), (-1,-1))

        it "s4" $ \_ -> do
          parseStar "position=<-3, -2> velocity=< 1, -1>"
              `shouldBe` ((-3, -2), (1,-1))

      describe "moveStar" $ do

        it "s1" $ \_ -> do
          moveStar ((9,1), (0,2))
            `shouldBe` ((9,3), (0,2))

        it "s2" $ \_ -> do
          moveStar ((-3,11), (1,-2))
            `shouldBe` ((-2,9), (1,-2))

      describe "countDistinctYs" $ do

        it "sample" $ \_ -> do
          countDistinctYs (parseInitialModel sample)
            `shouldBe` 15

        it "sample updated" $ \_ -> do
          countDistinctYs
            (updateModel $ parseInitialModel sample)
              `shouldBe` 11

        it "sample t3 number of ys" $ \_ -> do
          (countDistinctYs
            $ (!! 3)
            $ iterate updateModel
            $ parseInitialModel sample)
              `shouldBe` 8

        it "sample t4 number of ys" $ \_ -> do
          (countDistinctYs
            $ (!! 4)
            $ iterate updateModel
            $ parseInitialModel sample)
              `shouldBe` 11

      describe "findSecretMessageMoment" $ do

        it "secret message moment for sample" $ \_ -> do
          ((\(i, _, y) -> (i, y))
            . findSecretMessageMoment 10
            . parseInitialModel
            $ sample)
              `shouldBe` (3, 8)
