module Day24.Spec where

import Test.Hspec
import Text.ParserCombinators.ReadP
  ( readP_to_S )

import Day24.Day24

spec :: IO ()
spec = do
  specA

specA :: IO ()
specA = hspec $ do
  describe "Day24" $ do

    describe "solve" $ do

      it "sample" $ do
        let armies = readArmies sample
        solve armies `shouldBe` 5216

    describe "parseGroup" $ do

      it "Group is1" $ do
        let input = "4228 units each with 24924 hit points (weak to radiation, fire; immune to cold, bludgeoning) with an attack that does 11 cold damage at initiative 11"
        let expected =
              Group
              { units = 4228
              , hitPoints = 24924
              , weaknesses = [ Radiation, Fire ]
              , immunities = [ Cold, Bludgeoning ]
              , attack = Cold
              , damage = 11
              , initiative = 11
              , army = ImmuneSystem
              }
        parseGroupHelper ImmuneSystem input `shouldBe` expected

      it "Group is2" $ do
        let input = "6846 units each with 2773 hit points (weak to slashing, cold) with an attack that does 4 slashing damage at initiative 14"
        let expected =
              Group
              { units = 6846
              , hitPoints = 2773
              , weaknesses = [ Slashing, Cold ]
              , immunities = []
              , attack = Slashing
              , damage = 4
              , initiative = 14
              , army = ImmuneSystem
              }
        parseGroupHelper ImmuneSystem input `shouldBe` expected

      it "Group is3" $ do
        let input = "105 units each with 6988 hit points (weak to bludgeoning; immune to radiation) with an attack that does 616 radiation damage at initiative 17"
        let expected =
              Group
              { units = 105
              , hitPoints = 6988
              , weaknesses = [ Bludgeoning ]
              , immunities = [ Radiation ]
              , attack = Radiation
              , damage = 616
              , initiative = 17
              , army = ImmuneSystem
              }
        parseGroupHelper ImmuneSystem input `shouldBe` expected

      it "Group is4" $ do
        let input = "5615 units each with 7914 hit points (weak to bludgeoning) with an attack that does 13 radiation damage at initiative 20"
        let expected =
              Group
              { units = 5615
              , hitPoints = 7914
              , weaknesses = [ Bludgeoning ]
              , immunities = []
              , attack = Radiation
              , damage = 13
              , initiative = 20
              , army = ImmuneSystem
              }
        parseGroupHelper ImmuneSystem input `shouldBe` expected

      it "Group is5" $ do
        let input = "1021 units each with 10433 hit points (weak to cold; immune to slashing, bludgeoning) with an attack that does 86 bludgeoning damage at initiative 12"
        let expected =
              Group
              { units = 1021
              , hitPoints = 10433
              , weaknesses = [ Cold ]
              , immunities = [ Slashing, Bludgeoning ]
              , attack = Bludgeoning
              , damage = 86
              , initiative = 12
              , army = ImmuneSystem
              }
        parseGroupHelper ImmuneSystem input `shouldBe` expected

      it "Group is6" $ do
        let input = "6099 units each with 11578 hit points with an attack that does 15 bludgeoning damage at initiative 13"
        let expected =
              Group
              { units = 6099
              , hitPoints = 11578
              , weaknesses = []
              , immunities = []
              , attack = Bludgeoning
              , damage = 15
              , initiative = 13
              , army = ImmuneSystem
              }
        parseGroupHelper ImmuneSystem input `shouldBe` expected

      it "Group is7" $ do
        let input = "82 units each with 1930 hit points (weak to bludgeoning; immune to cold) with an attack that does 179 bludgeoning damage at initiative 5"
        let expected =
              Group
              { units = 82
              , hitPoints = 1930
              , weaknesses = [ Bludgeoning ]
              , immunities = [ Cold ]
              , attack = Bludgeoning
              , damage = 179
              , initiative = 5
              , army = ImmuneSystem
              }
        parseGroupHelper ImmuneSystem input `shouldBe` expected

      it "Group is8" $ do
        let input = "2223 units each with 9442 hit points (immune to bludgeoning) with an attack that does 38 cold damage at initiative 19"
        let expected =
              Group
              { units = 2223
              , hitPoints = 9442
              , weaknesses = []
              , immunities = [ Bludgeoning ]
              , attack = Cold
              , damage = 38
              , initiative = 19
              , army = ImmuneSystem
              }
        parseGroupHelper ImmuneSystem input `shouldBe` expected

      it "Group is9" $ do
        let input = "140 units each with 7594 hit points (weak to radiation) with an attack that does 452 fire damage at initiative 8"
        let expected =
              Group
              { units = 140
              , hitPoints = 7594
              , weaknesses = [ Radiation ]
              , immunities = []
              , attack = Fire
              , damage = 452
              , initiative = 8
              , army = ImmuneSystem
              }
        parseGroupHelper ImmuneSystem input `shouldBe` expected

      it "Group is10" $ do
        let input = "3057 units each with 3871 hit points (weak to bludgeoning) with an attack that does 11 radiation damage at initiative 16"
        let expected =
              Group
              { units = 3057
              , hitPoints = 3871
              , weaknesses = [ Bludgeoning ]
              , immunities = []
              , attack = Radiation
              , damage = 11
              , initiative = 16
              , army = ImmuneSystem
              }
        parseGroupHelper ImmuneSystem input `shouldBe` expected

      it "Group inf1" $ do
        let input = "263 units each with 48098 hit points (immune to radiation; weak to slashing) with an attack that does 293 bludgeoning damage at initiative 2"
        let expected =
              Group
              { units = 263
              , hitPoints = 48098
              , weaknesses = [ Slashing ]
              , immunities = [ Radiation ]
              , attack = Bludgeoning
              , damage = 293
              , initiative = 2
              , army = Infection
              }
        parseGroupHelper Infection input `shouldBe` expected

      it "Group inf2" $ do
        let input = "111 units each with 9893 hit points (immune to slashing) with an attack that does 171 fire damage at initiative 18"
        let expected =
              Group
              { units = 111
              , hitPoints = 9893
              , weaknesses = []
              , immunities = [ Slashing ]
              , attack = Fire
              , damage = 171
              , initiative = 18
              , army = Infection
              }
        parseGroupHelper Infection input `shouldBe` expected

      it "Group inf3" $ do
        let input = "2790 units each with 36205 hit points with an attack that does 25 cold damage at initiative 4"
        let expected =
              Group
              { units = 2790
              , hitPoints = 36205
              , weaknesses = []
              , immunities = []
              , attack = Cold
              , damage = 25
              , initiative = 4
              , army = Infection
              }
        parseGroupHelper Infection input `shouldBe` expected

      it "Group inf4" $ do
        let input = "3325 units each with 46479 hit points (weak to slashing) with an attack that does 27 radiation damage at initiative 1"
        let expected =
              Group
              { units = 3325
              , hitPoints = 46479
              , weaknesses = [ Slashing ]
              , immunities = []
              , attack = Radiation
              , damage = 27
              , initiative = 1
              , army = Infection
              }
        parseGroupHelper Infection input `shouldBe` expected

      it "Group inf5" $ do
        let input = "3593 units each with 6461 hit points (weak to fire, slashing) with an attack that does 3 radiation damage at initiative 15"
        let expected =
              Group
              { units = 3593
              , hitPoints = 6461
              , weaknesses = [ Fire, Slashing ]
              , immunities = []
              , attack = Radiation
              , damage = 3
              , initiative = 15
              , army = Infection
              }
        parseGroupHelper Infection input `shouldBe` expected

      it "Group inf6" $ do
        let input = "2925 units each with 13553 hit points (weak to cold, bludgeoning; immune to fire) with an attack that does 8 cold damage at initiative 10"
        let expected =
              Group
              { units = 2925
              , hitPoints = 13553
              , weaknesses = [ Cold, Bludgeoning ]
              , immunities = [ Fire ]
              , attack = Cold
              , damage = 8
              , initiative = 10
              , army = Infection
              }
        parseGroupHelper Infection input `shouldBe` expected

      it "Group inf7" $ do
        let input = "262 units each with 43260 hit points (weak to cold) with an attack that does 327 radiation damage at initiative 6"
        let expected =
              Group
              { units = 262
              , hitPoints = 43260
              , weaknesses = [ Cold ]
              , immunities = []
              , attack = Radiation
              , damage = 327
              , initiative = 6
              , army = Infection
              }
        parseGroupHelper Infection input `shouldBe` expected

      it "Group inf8" $ do
        let input = "4228 units each with 24924 hit points (weak to radiation, fire; immune to cold, bludgeoning) with an attack that does 11 cold damage at initiative 11"
        let expected =
              Group
              { units = 4228
              , hitPoints = 24924
              , weaknesses = [ Radiation, Fire ]
              , immunities = [ Cold, Bludgeoning ]
              , attack = Cold
              , damage = 11
              , initiative = 11
              , army = Infection
              }
        parseGroupHelper Infection input `shouldBe` expected

      it "Group inf9" $ do
        let input = "689 units each with 42315 hit points (weak to cold, slashing) with an attack that does 116 fire damage at initiative 7"
        let expected =
              Group
              { units = 689
              , hitPoints = 42315
              , weaknesses = [ Cold, Slashing ]
              , immunities = []
              , attack = Fire
              , damage = 116
              , initiative = 7
              , army = Infection
              }
        parseGroupHelper Infection input `shouldBe` expected

      it "Group inf10" $ do
        let input = "2649 units each with 37977 hit points (weak to radiation) with an attack that does 24 cold damage at initiative 3"
        let expected =
              Group
              { units = 2649
              , hitPoints = 37977
              , weaknesses = [ Radiation ]
              , immunities = []
              , attack = Cold
              , damage = 24
              , initiative = 3
              , army = Infection
              }
        parseGroupHelper Infection input `shouldBe` expected


      it "Sample g1" $ do
        let input = "17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2"
        let expected =
              Group
              { units = 17
              , hitPoints = 5390
              , weaknesses = [ Radiation, Bludgeoning ]
              , immunities = []
              , attack = Fire
              , damage = 4507
              , initiative = 2
              , army = ImmuneSystem
              }
        parseGroupHelper ImmuneSystem input `shouldBe` expected

      it "Sample g2" $ do
        let input = "989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3"
        let expected =
              Group
              { units = 989
              , hitPoints = 1274
              , weaknesses = [ Bludgeoning, Slashing ]
              , immunities = [ Fire ]
              , attack = Slashing
              , damage = 25
              , initiative = 3
              , army = ImmuneSystem
              }
        parseGroupHelper ImmuneSystem input `shouldBe` expected

      it "Sample g3" $ do
        let input = "801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1"
        let expected =
              Group
              { units = 801
              , hitPoints = 4706
              , weaknesses = [ Radiation ]
              , immunities = [ ]
              , attack = Bludgeoning
              , damage = 116
              , initiative = 1
              , army = Infection
              }
        parseGroupHelper Infection input `shouldBe` expected

      it "Sample g4" $ do
        let input = "4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4"
        let expected =
              Group
              { units = 4485
              , hitPoints = 2961
              , weaknesses = [ Fire, Cold ]
              , immunities = [ Radiation ]
              , attack = Slashing
              , damage = 12
              , initiative = 4
              , army = Infection
              }
        parseGroupHelper Infection input `shouldBe` expected

    -- fromList [(6,Group {units = 2665, hitPoints = 11578, weaknesses = [], immunities = [], attack = Bludgeoning, damage = 46, initiative = 13, army = ImmuneSystem}),(18,Group {units = 752, hitPoints = 24924, weaknesses = [Radiation,Fire], immunities = [Cold,Bludgeoning], attack = Cold, damage = 11, initiative = 11, army = Infection})]


parseGroupHelper
  :: Army
  -> String
  -> Group
parseGroupHelper army =
  fst
  . last
  . readP_to_S (parseGroup army)

