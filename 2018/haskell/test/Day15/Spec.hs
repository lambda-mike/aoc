module Day15.Spec where

import qualified Data.Map.Strict as M
import Prelude hiding (round)
import Test.Hspec

import Day15.Day15 as Day15

spec :: IO ()
spec = do
  specA

specA :: IO ()
specA = hspec $ do

  describe "Day15A" $ do

    describe "solve" $ do

      it "combat1" $ do

        Day15.solve Day15.combat1 `shouldBe` 36334

      it "combat2" $ do

        Day15.solve Day15.combat2 `shouldBe` 39514

      it "combat3" $ do

        Day15.solve Day15.combat3 `shouldBe` 27755

      it "combat4" $ do

        Day15.solve Day15.combat4 `shouldBe` 28944

      it "combat5" $ do

        Day15.solve Day15.combat5 `shouldBe` 18740

    describe "isBattleOver" $ do

      it "only Elves remain" $ do

        let elvesOnly = map (\c -> if c == 'G' then 'E' else c) Day15.combat1

        let battle
              = initBattle
              . parseBattlefield Day15.initialPower
              $ elvesOnly

        battle `shouldSatisfy` Day15.isBattleOver

      it "only Goblins remain" $ do

        let goblinsOnly = map (\c -> if c == 'E' then 'G' else c) Day15.combat1

        let battle
              = initBattle
              . parseBattlefield Day15.initialPower
              $ goblinsOnly

        battle `shouldSatisfy` Day15.isBattleOver

      it "both Elves and Goblins remain" $ do

        let battle
              = initBattle
              . parseBattlefield Day15.initialPower
              $ Day15.combat1

        battle `shouldNotSatisfy` Day15.isBattleOver

    describe "calculateOutcome" $ do

      it "calculates outcome for elves" $ do

        let battlefield' = "#######\n#...#E#\n#E#...#\n#.E##.#\n#E..#E#\n#.....#\n#######"

        let battle
              = (\b -> b { round = 37 })
              . initBattle
              . M.insert (5,1) (Elf $
                Unit { power = 3
                     , hitPoints = 200
                     , position = (5,1)
                     }
              )
              . M.insert (1,2) (Elf $
                Unit { power = 3
                     , hitPoints = 197
                     , position = (1,2)
                     }
              )
              . M.insert (2,3) (Elf $
                Unit { power = 3
                     , hitPoints = 185
                     , position = (2,3)
                     }
              )
              . M.insert (1,4) (Elf $
                Unit { power = 3
                     , hitPoints = 200
                     , position = (1,4)
                     }
              )
              . M.insert (5,4) (Elf $
                Unit { power = 3
                     , hitPoints = 200
                     , position = (5,4)
                     }
              )
              . parseBattlefield Day15.initialPower
              $ battlefield'

        Day15.calculateOutcome battle `shouldBe` 36334

      it "calculates outcome for goblins" $ do

        let battlefield' = "#########\n#.G.....#\n#G.G#...#\n#.G##...#\n#...##..#\n#.G.#...#\n#.......#\n#.......#\n#########"

        let battle
              = (\b -> b { round = 20 })
              . initBattle
              . M.insert (2,1) (Goblin $
                Unit { power = 3
                     , hitPoints = 137
                     , position = (2,1)
                     }
              )
              . M.insert (1,2) (Goblin $
                Unit { power = 3
                     , hitPoints = 200
                     , position = (1,2)
                     }
              )
              . M.insert (3,2) (Goblin $
                Unit { power = 3
                     , hitPoints = 200
                     , position = (3,2)
                     }
              )
              . M.insert (2,3) (Goblin $
                Unit { power = 3
                     , hitPoints = 200
                     , position = (2,3)
                     }
              )
              . M.insert (2,5) (Goblin $
                Unit { power = 3
                     , hitPoints = 200
                     , position = (2,5)
                     }
              )
              . parseBattlefield Day15.initialPower
              $ battlefield'

        Day15.calculateOutcome battle `shouldBe` 18740

    describe "orderUnits" $ do

      it "combat2" $ do

        let battle
              = initBattle
              . parseBattlefield Day15.initialPower
              $ Day15.combat2
        let orderedPositions =
              map
                (\c ->
                  case c of
                    Cavern -> error "cavern"
                    Wall -> error "wall"
                    Elf u -> position u
                    Goblin u -> position u)
                $ orderUnits battle
        let positions =
              [(1,1), (4,1), (5,1), (3,2), (5,2), (1,3), (5,3), (1, 4), (3,5)]

        orderedPositions `shouldBe` positions

    describe "findAdjacentEnemy" $ do

      it "takes in reading order when tie" $ do

        let attacker = Goblin (Unit 3 200 (5,1))
            enemy = Elf (Unit 3 200 (4,1))
            battlefield' = parseBattlefield Day15.initialPower Day15.combat2

        Day15.findAdjacentEnemy
          attacker
          battlefield'
          `shouldBe`
            (Just enemy)

      it "takes enemy with less hit points" $ do

        let attacker = Goblin (Unit 3 200 (5,1))
            enemy = Elf (Unit 3 190 (5,2))
            battlefield' =
              M.insert
                (5,2)
                enemy
                (parseBattlefield Day15.initialPower Day15.combat2)

        Day15.findAdjacentEnemy
          attacker
          battlefield'
          `shouldBe`
            (Just enemy)

    describe "move" $ do

      it "combat2 E(1,1) chooses the only available path" $ do

        let attacker = Elf (Unit 3 200 (1,1))
            movedAttacker = Elf (Unit 3 200 (2,1))
            battlefield' = parseBattlefield Day15.initialPower Day15.combat2
            updatedBattlefield =
              M.insert (1,1) Cavern $
                M.insert (2,1) movedAttacker $
                  battlefield'

        Day15.move
          attacker
          battlefield'
          `shouldBe`
            (movedAttacker, updatedBattlefield)

      it "combat2 G(3,2) chooses equal path in reading order" $ do

        let attacker = Goblin (Unit 3 200 (3,2))
            movedAttacker = Goblin (Unit 3 200 (3,1))
            battlefield' = parseBattlefield Day15.initialPower Day15.combat2
            updatedBattlefield =
              M.insert (3,2) Cavern $
                M.insert (3,1) movedAttacker $
                  battlefield'

        Day15.move
          attacker
          battlefield'
          `shouldBe`
            (movedAttacker, updatedBattlefield)

      it "combat2 E(5,3) does not move when no access to enemies" $ do

        let attacker = Elf (Unit 3 200 (5,3))
            battlefield' = parseBattlefield Day15.initialPower Day15.combat2

        Day15.move
          attacker
          battlefield'
          `shouldBe`
            (attacker, battlefield')

  describe "Day15B" $ do

    describe "solveB" $ do

      it "combat1b" $ do

        Day15.solveB Day15.combat1b `shouldBe` 4988

      it "combat2b" $ do

        Day15.solveB Day15.combat2b `shouldBe` 31284

      it "combat3b" $ do

        Day15.solveB Day15.combat3b `shouldBe` 3478

      it "combat4b" $ do

        Day15.solveB Day15.combat4b `shouldBe` 6474

      it "combat5b" $ do

        Day15.solveB Day15.combat5b `shouldBe` 1140
