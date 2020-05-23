module Day07.Spec where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Test.Hspec

import Day07.Day07

spec :: IO ()
spec = hspec $ do

  beforeAll (readFile "src/Day07/input.txt") $ do

    describe "Day07" $ do

      describe "parseDependency" $ do

        it "line" $ \_ -> do
          parseDependency line
            `shouldBe`
              [ ('A', 'C'), ('C', blank) ]

      describe "parseDependencies" $ do

        it "sample" $ \_ -> do
          parseDependencies (lines sample)
            `shouldBe`
              [ ('A', 'C')
              , ('C', blank)
              , ('F', 'C')
              , ('C', blank)
              , ('B', 'A')
              , ('A', blank)
              , ('D', 'A')
              , ('A', blank)
              , ('E', 'B')
              , ('B', blank)
              , ('E', 'D')
              , ('D', blank)
              , ('E', 'F')
              , ('F', blank)
              ]

      describe "orderSteps" $ do

        it "sample" $ \_ -> do
          let result =
                orderSteps
                . createStepsMap
                . parseDependencies
                $ lines sample
          result `shouldBe` "CABDFE"

      describe "addFinalSteps" $ do

        it "ABCDEF" $ \_ -> do
          addFinalSteps
            "DEF"
            (S.fromList "ABC")
              `shouldBe` "ABCDEF"
