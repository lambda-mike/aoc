module Day19.Spec where

import qualified Data.Vector as V
import Test.Hspec
import Day19.Day19 as Day19

spec :: IO ()
spec = do
  specA

specA :: IO ()
specA = hspec $ do

  describe "Day19" $ do

    beforeAll (readFile "src/Day19/input.txt") $ do

      describe "solve" $ do

        describe "Given sample input" $ do

          it "calculates result" $ \_ -> do

            Day19.solve (V.replicate 6 0) sample19 `shouldBe` 6

      describe "solveB" $ do

        it "calculates result for partA" $ \input -> do

          Day19.solveB
            (V.replicate 6 0)
            input
            `shouldBe` 1248
