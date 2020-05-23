module DaySpec where

import Test.Hspec

import DayLib as Day

spec :: IO ()
spec = do
  specA

specA :: IO ()
specA = hspec $ do
  describe "Day" $ do

    describe "solve" $ do

      describe "TODO" $ do
        it "TODO" $ do
          Day.solve () `shouldBe` ()

