module Day17.Spec where

import Test.Hspec

import Day17.Day17 as Day17

spec :: IO ()
spec = do
  specA

specA :: IO ()
specA = hspec $ do

  describe "Day17" $ do

    describe "solve" $ do

      it "sample" $ do
        let springs = [((500, 0), Day17.Down)]
        Day17.solve Day17.d17sample springs `shouldBe` 57

      it "one cell width" $ do
        let springs = [((500, 0), Day17.Down)]
        Day17.solve Day17.d17oneCellWidth springs `shouldBe` 14

      it "two stream symmetric one reservoir" $ do
        let springs = [((498,0), Down), ((502,0), Down)]
        Day17.solve Day17.d17oneReservoir springs `shouldBe` 39

      it "one stream on box in the reservoir" $ do
        let springs = [((500,0), Down)]
        Day17.solve Day17.d17reservoirWithBox springs `shouldBe` 47
