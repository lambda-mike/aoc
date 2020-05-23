{-# LANGUAGE OverloadedStrings #-}

module Day20.Spec where

import Test.Hspec

import Day20.Day20 as Day20

spec :: IO ()
spec = do
  specA

specA :: IO ()
specA = hspec $ do
  describe "Day20" $ do

    describe "solve" $ do

      it "^WNE$" $ do
        Day20.solve "^WNE$\n" `shouldBe` 3

      it "^N(E|WW)N$" $ do
        Day20.solve "^N(E|WW)N$\n" `shouldBe` 4

      it "WWWNNEESSS" $ do
        Day20.solve "^WWWNNEESSS$\n" `shouldBe` 5

      it "WWWNNEESSSEENNWS" $ do
        Day20.solve "^WWWNNEESSSEENNWS$\n" `shouldBe` 5


      it "^ENWWW(NEEE|SSE(EE|N))$" $ do
        Day20.solve "^ENWWW(NEEE|SSE(EE|N))$\n" `shouldBe` 10

      it "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$" $ do
        Day20.solve "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$\n" `shouldBe` 18

      it "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$" $ do
        Day20.solve "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$\n" `shouldBe` 23

      it "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$" $ do
        Day20.solve "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$\n" `shouldBe` 31

