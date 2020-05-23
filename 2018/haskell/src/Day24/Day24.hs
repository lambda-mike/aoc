module Day24.Day24
  ( Army(..)
  , AttackType(..)
  , Group(..)
  , parseArmy
  , parseAttack
  , parseAttackTypes
  , parseGroup
  , parseNumber
  , parseWeaknesses
  , parseWeakAndImmunity
  , readArmies
  , solve
  , solveB
  , d24input
  , sample
  -- private
  , buildArmiesMap
  , noWinner
  , fight
  , debug
  , selectTargets
  , calculateTargets
  , attackTargets
  , findUpperBoundBoost
  ) where

import Data.Char (isDigit)
import Data.Function ((&))
import qualified Data.List as L
  ( filter
  , foldl'
  , last
  , reverse
  , sortBy
  )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.ParserCombinators.ReadP
  ( ReadP
  , (+++)
  , between
  , char
  , many1
  , munch1
  , option
  , optional
  , readP_to_S
  , satisfy
  , sepBy1
  , skipSpaces
  , string
  )
import Text.ParserCombinators.ReadPrec (lift)
import Text.Read (Read(readPrec))

import Debug.Trace

-- 4228 units each with 24924 hit points (weak to radiation, fire; immune to cold, bludgeoning) with an attack that does 11 cold damage at initiative 11

data Army
  = ImmuneSystem
  | Infection
  deriving (Eq, Show)

data Group = Group
  { units :: Int
  , hitPoints :: HP
  , weaknesses :: [AttackType]
  , immunities :: [AttackType]
  , attack :: AttackType
  , damage :: Damage
  , initiative :: Initiative
  , army :: Army
  }
  deriving (Eq, Show)

data AttackType
  = Bludgeoning
  | Cold
  | Fire
  | Radiation
  | Slashing
  deriving (Eq, Show)

instance Read AttackType where
  readPrec = lift parseAttack

type HP = Int
type Damage = Int
type Initiative = Int
type GroupId = Int

-- 16086
solve
  :: [Group]
  -> Int -- Units number of the winning army
solve
  = countUnits
  . startBattle
  . buildArmiesMap

-- 3957
solveB
  :: [Group]
  -> Int -- Units number of the winning army
solveB
  = countUnits
  . findOptimalBoost
  . buildArmiesMap

findOptimalBoost
  :: Map GroupId Group
  -> Map GroupId Group
findOptimalBoost armies =
  let upperBoundBoost = findUpperBoundBoost armies
      lowerBoundBoost = 0
      optimalBoost =
        bijectByBoost
          lowerBoundBoost
          upperBoundBoost
          armies
  in startBattleWithBoost optimalBoost armies

bijectByBoost
  :: Int
  -> Int
  -> Map GroupId Group
  -> Int
bijectByBoost lower upper armies =
  if upper - lower <= 1
  then upper
  else
    let middleBoost =
           (lower + upper) `div` 2
        middleBattleResult =
          startBattleWithBoost
            middleBoost
            armies
    in if noInfection middleBattleResult
       then bijectByBoost lower middleBoost armies
       else bijectByBoost middleBoost upper armies

startBattleWithBoost
  :: Int
  -> Map GroupId Group
  -> Map GroupId Group
startBattleWithBoost boost =
  startBattle
  . boostWith boost

boostWith
  :: Int
  -> Map GroupId Group
  -> Map GroupId Group
boostWith boost =
  Map.map (boostImmuneSystem boost)
  where
    boostImmuneSystem boost group =
      if ImmuneSystem == army group
      then group { damage = boost + damage group }
      else group

findUpperBoundBoost
  :: Map GroupId Group
  -> Int
findUpperBoundBoost armies =
  let doses = [ 2 ^ x | x <- [0..] ]
      battleResults =
        map (flip startBattleWithBoost armies) doses
  in fst $
     head $
     filter (noInfection . snd) $
     zip doses battleResults

noInfection
  :: Map GroupId Group
  -> Bool
noInfection =
  all ((== ImmuneSystem) . army . snd)
  . Map.toList

countUnits
  :: Map GroupId Group
  -> Int
countUnits =
  Map.foldr'
    ((+) . units)
    0

noWinner
  :: Map GroupId Group
  -> Bool
noWinner groups =
  any (hasUnits ImmuneSystem) groups
  &&
  any (hasUnits Infection) groups
  where
    hasUnits armyType group =
      armyType == army group
      &&
      0 < units group

startBattle
  :: Map GroupId Group
  -> Map GroupId Group
startBattle armies =
  startBattle' armies Map.empty
  where
    startBattle' armies prevArmies
      | armies == prevArmies = armies
      | isBattleOver armies  = armies
      | otherwise            = startBattle' (fight armies) armies

isBattleOver
  :: Map GroupId Group
  -> Bool
isBattleOver groups =
  all ((ImmuneSystem ==) . army) groups
  ||
  all ((Infection ==) . army) groups

fight
  :: Map GroupId Group
  -> Map GroupId Group
fight armies =
  attackTargets armies
  . selectTargets
  . Map.toList
  $ armies

selectTargets
  :: [(GroupId, Group)]
  -> [(GroupId, GroupId)]
selectTargets groupsList =
  let attackers =
        L.sortBy
          (\(_, x) (_, y) -> compare (attackerIndex x) (attackerIndex y))
          groupsList
      targets =
        L.reverse $
          selectTargets' groupsList attackers []
  in targets
  where
    attackerIndex group
      = ( -(effectivePower group)
        , -(initiative group)
        )
    selectTargets' _ [] battlePlan = battlePlan
    selectTargets' allGroups ((aid, attacker) : attackers) battlePlan =
      let target =
            calculateTargets attacker allGroups (map snd battlePlan)
      in case target of
          [] ->
            selectTargets' allGroups attackers battlePlan
          [tid] ->
            selectTargets'
              allGroups
              attackers
              ((aid, tid) : battlePlan)

calculateTargets
  :: Group
  -> [(GroupId, Group)]
  -> [GroupId]
  -> [GroupId]
calculateTargets attacker groups targetsSoFar =
  map fst
  . take 1
  . filter (\gg ->
      ((0 <) . calculateDamage attacker . snd $ gg)
      -- &&
      -- ((0 <) . units . snd $ gg)
    )
  . L.sortBy
      (\(_, gx) (_, gy) ->
        compare
          (calculateTargetIndex attacker gx)
          (calculateTargetIndex attacker gy))
  . filter (not . (\aid -> any (== aid) targetsSoFar) . fst)
  . filter (isEnemy attacker . snd)
  $ groups

calculateTargetIndex
  attacker
  target =
  ( -(calculateDamage attacker target)
  , -(effectivePower target)
  , -(initiative target)
  )

calculateDamage
  :: Group
  -> Group
  -> Int
calculateDamage attacker defender
  | any (== attackType) (immunities defender) = 0
  | any (== attackType) (weaknesses defender) =
      2 * effectivePower attacker
  | otherwise =
       effectivePower attacker
  where
    attackType = attack attacker

isEnemy g1 g2 =
  army g1 /= army g2

effectivePower
  :: Group
  -> Int
effectivePower
  group =
  (units group) * (damage group)

attackTargets
  :: Map GroupId Group
  -> [(GroupId, GroupId)]
  -> Map GroupId Group
attackTargets groups =
  Map.filter ((> 0) . units)
  . L.foldl' attackTarget groups
  . L.sortBy (\(x,_) (y, _) ->
      compare
        (-(lookupInitiative groups x))
        (-(lookupInitiative groups y))
    )
  where
    lookupInitiative groups =
      initiative . (groups Map.!)
    attackTarget
      :: Map GroupId Group
      -> (GroupId, GroupId)
      -> Map GroupId Group
    attackTarget armies (attackerId, targetId) =
      let attacker = armies Map.! attackerId
      in
        if 0 == (units attacker)
        then armies
        else Map.adjust (killUnits attacker) targetId armies

killUnits
  :: Group
  -> Group
  -> Group
killUnits attacker defender =
  let damage =
        calculateDamage attacker defender
      killedUnits =
        damage `div` (hitPoints defender)
      remainingUnits =
        max 0 (units defender - killedUnits)
  in defender { units = remainingUnits }

buildArmiesMap
  :: [Group]
  -> Map Int Group
buildArmiesMap =
  Map.fromList
  . zip [1..]

readArmies
  :: String
  -> [Group]
readArmies =
  fst
  . L.last
  . readP_to_S parseArmies

parseArmies
  :: ReadP [Group]
parseArmies = do
  string "Immune System:\n"
  immuneSystemArmy <-
    parseArmy ImmuneSystem
  string "\n\nInfection:\n"
  infectionArmy <-
    parseArmy Infection
  return $
    immuneSystemArmy
    ++
    infectionArmy

parseArmy
  :: Army
  -> ReadP [Group]
parseArmy army = do
  sepBy1
    (parseGroup army)
    (string "\n")

parseGroup
  :: Army
  -> ReadP Group
parseGroup army = do
  units <- parseNumber
  string " units each with "
  hitPoints <- parseNumber
  string " hit points "
  (weaknesses, immunities)
    <- parseWeakAndImmunity
  string "with an attack that does "
  damage <- parseNumber
  string " "
  attack <- parseAttack
  string " damage at initiative "
  initiative <- parseNumber
  return $ Group
    units
    hitPoints
    weaknesses
    immunities
    attack
    damage
    initiative
    army

parseNumber
  :: ReadP Int
parseNumber = do
  num <- munch1 isDigit
  return $
    read num

parseAttack
  :: ReadP AttackType
parseAttack = do
  parseBludgeoning
  +++
  parseCold
  +++
  parseFire
  +++
  parseRadiation
  +++
  parseSlashing
  where
    parseBludgeoning =
      string "bludgeoning"
      >> pure Bludgeoning
    parseCold =
      string "cold"
      >> pure Cold
    parseFire =
      string "fire"
      >> pure Fire
    parseRadiation =
      string "radiation"
      >> pure Radiation
    parseSlashing =
      string "slashing"
      >> pure Slashing

parseWeakAndImmunity
  :: ReadP ([AttackType], [AttackType])
parseWeakAndImmunity =
  option
    ([], [])
    (between
      (char '(')
      (string ") ")
      (weakThenImmune
      +++
      immuneThenWeak)
      )
  where
    weakThenImmune = do
      weaknesses <- option [] parseWeaknesses
      immunities <- option [] parseImmunities
      return (weaknesses, immunities)
    immuneThenWeak = do
      immunities <- option [] parseImmunities
      weaknesses <- option [] parseWeaknesses
      return (weaknesses, immunities)

parseWeaknesses :: ReadP [AttackType]
parseWeaknesses = do
  string "weak to "
  parseAttackTypes

parseImmunities :: ReadP [AttackType]
parseImmunities = do
  string "immune to "
  parseAttackTypes

parseAttackTypes :: ReadP [AttackType]
parseAttackTypes = do
  attacks
    <- many1 (satisfy (\c -> c /= ';' && c /= ')'))
  optional (string "; ")
  return $
    attacks &
    L.filter (/= ' ') &
    fmap (\c ->
      if c == ','
      then ' '
      else c) &
    words &
    fmap read

debug :: IO ()
debug = do
  let armies = buildArmiesMap $ readArmies sample
  print armies
  let f1 = fight armies
  print $ noWinner f1
  let f2 = fight f1
  print $ noWinner f2
  let f3 = fight f2
  print $ noWinner f3
  let f4 = fight f3
  print $ noWinner f4
  let f5 = fight f4
  print $ noWinner f5
  let f6 = fight f5
  print $ noWinner f6
  let f7 = fight f6
  print $ noWinner f7
  let f8 = fight f7
  print $ noWinner f8
  print f8
  print $ countUnits f8


d24input = "Immune System:\n698 units each with 10286 hit points with an attack that does 133 fire damage at initiative 9\n6846 units each with 2773 hit points (weak to slashing, cold) with an attack that does 4 slashing damage at initiative 14\n105 units each with 6988 hit points (weak to bludgeoning; immune to radiation) with an attack that does 616 radiation damage at initiative 17\n5615 units each with 7914 hit points (weak to bludgeoning) with an attack that does 13 radiation damage at initiative 20\n1021 units each with 10433 hit points (weak to cold; immune to slashing, bludgeoning) with an attack that does 86 bludgeoning damage at initiative 12\n6099 units each with 11578 hit points with an attack that does 15 bludgeoning damage at initiative 13\n82 units each with 1930 hit points (weak to bludgeoning; immune to cold) with an attack that does 179 bludgeoning damage at initiative 5\n2223 units each with 9442 hit points (immune to bludgeoning) with an attack that does 38 cold damage at initiative 19\n140 units each with 7594 hit points (weak to radiation) with an attack that does 452 fire damage at initiative 8\n3057 units each with 3871 hit points (weak to bludgeoning) with an attack that does 11 radiation damage at initiative 16\n\nInfection:\n263 units each with 48098 hit points (immune to radiation; weak to slashing) with an attack that does 293 bludgeoning damage at initiative 2\n111 units each with 9893 hit points (immune to slashing) with an attack that does 171 fire damage at initiative 18\n2790 units each with 36205 hit points with an attack that does 25 cold damage at initiative 4\n3325 units each with 46479 hit points (weak to slashing) with an attack that does 27 radiation damage at initiative 1\n3593 units each with 6461 hit points (weak to fire, slashing) with an attack that does 3 radiation damage at initiative 15\n2925 units each with 13553 hit points (weak to cold, bludgeoning; immune to fire) with an attack that does 8 cold damage at initiative 10\n262 units each with 43260 hit points (weak to cold) with an attack that does 327 radiation damage at initiative 6\n4228 units each with 24924 hit points (weak to radiation, fire; immune to cold, bludgeoning) with an attack that does 11 cold damage at initiative 11\n689 units each with 42315 hit points (weak to cold, slashing) with an attack that does 116 fire damage at initiative 7\n2649 units each with 37977 hit points (weak to radiation) with an attack that does 24 cold damage at initiative 3\n"

sample :: String
sample = "Immune System:\n17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2\n989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3\n\nInfection:\n801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1\n4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4"

