module Day09.Spec where

import qualified Data.Vector.Unboxed as V
import Test.Hspec

import Day09.Day09 as Day09

spec :: IO ()
spec = do
  specA

specA :: IO ()
specA = hspec $ do

  describe "Day09A" $ do

    describe "solve" $ do

      it "9 25" $ do

        Day09.solve 9 25 `shouldBe` 32

      it "9 45" $ do

        Day09.solve 9 45 `shouldBe` 32

      it "10 1618" $ do

        Day09.solve 10 1618 `shouldBe` 8317

      it "13 7999" $ do

        Day09.solve 13 7999 `shouldBe` 146373

      it "17 1104" $ do

        Day09.solve 17 1104 `shouldBe` 2764

      it "21 6111" $ do

        Day09.solve 21 6111 `shouldBe` 54718

      it "30 5807" $ do

        Day09.solve 30 5807 `shouldBe` 37305

