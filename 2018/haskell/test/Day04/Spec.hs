module Day04.Spec where

import Test.Hspec

import Day04.Day04

spec :: IO ()
spec = hspec $ do

  describe "Day04" $ do

    describe "parseEvent" $ do

      it "start event" $ do
        parseEvent startEvt
          `shouldBe`
            Event
              { date = "[1518-11-01 00:00]"
              , event = Start 10
              }

      it "sleep event" $ do
        parseEvent sleepEvt
          `shouldBe`
            Event
              { date = "[1518-11-01 00:05]"
              , event = Sleep 5
              }

      it "sleep event" $ do
        parseEvent wakeUpEvt
          `shouldBe`
            Event
              { date = "[1518-11-01 00:25]"
              , event = WakeUp 25
              }

    describe "solveA" $ do

      it "returns correct result for sample" $ do
        solveA sample
          `shouldBe` 240

    describe "solveB" $ do

      it "returns correct reult for sample" $ do
        solveB sample
          `shouldBe` 4455
