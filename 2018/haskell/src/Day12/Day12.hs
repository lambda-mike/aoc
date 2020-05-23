module Day12.Day12 where

import qualified Data.Bits as Bits
import qualified Data.Foldable as Foldable
import qualified Data.Function as F
import qualified Data.List as L
import qualified Data.Vector.Unboxed as V

type Pot = Integer
type Pots = V.Vector Int

data Plantation =
  Plantation
  { generation     :: !Integer
  -- 32 spread outcomes sorted by patterns of length 5 each
  , rules          :: !Pots
  , zeroIndexPos   :: !Int
  }

genNumA :: Int
genNumA = 20

genNumB :: Int
genNumB = 50000000000

parseInput :: String -> Plantation
parseInput input =
  let
    genStr = head . drop 2 . words . head . lines $ input
    rulesStr = drop 2 . lines $ input
    generation' = parsePots genStr
    rules =
      parseRules rulesStr
  in
    Plantation
    { generation     = generation'
    , rules          = rules
    , zeroIndexPos   = length genStr - 1
    }

parsePots :: String -> Integer
parsePots =
  snd .
  Foldable.foldr'
    (\p (i, gen) ->
      (i+1, Bits.shiftL (parsePot p) i Bits..|. gen))
    (0, 0)

parsePot :: Char -> Pot
parsePot '.' = 0
parsePot '#' = 1

parseRules :: [String] -> Pots
parseRules =
  V.fromList
  . map (fromInteger . snd)
  . L.sortBy (compare `F.on` fst)
  . fillInMissingPatterns -- only for sample
  . map parseRule
  where
    fillInMissingPatterns
      :: [(Integer, Pot)] -> [(Integer, Pot)]
    fillInMissingPatterns patterns =
      L.unionBy
        ((==) `F.on` fst)
        patterns
        [ (x, 0) | x <- [ 0 .. 31 ] ]

parseRule :: String -> (Integer, Pot)
parseRule input =
  let parts = words input
      pot = parsePot . head . head . drop 2 $ parts
      pattern = head $ parts
      -- reverse, because those are unsigned words
  in  (fromInteger $ parsePots pattern, pot)

simulate :: Int -> Plantation -> Plantation
simulate n plantation =
  go n 0 plantation
  where
    go n i plantation
      | i >= n =
          plantation
      | otherwise =
          go n (i+1) (growNextGen plantation)
    -- dbg n pln = if (n+1-1) `mod` 1000 == 0
    --         then traceShow
    --               ( n
    --               , sumPlantsIndexes pln
    --               , generation pln
    --               )
    --               n
    --         else n

growNextGen :: Plantation -> Plantation
growNextGen =
  removeRedundantPots .
  executeRules .
  addPotsToEnd

executeRules :: Plantation -> Plantation
executeRules plantation =
  plantation
    { generation =
        executeRules' generation' 0 0
    -- two bits most to the right are just to fill the pattern
    -- of 5 bits
    , zeroIndexPos = zeroIndexPos' - 2
    }
  where
    executeRules' gen i newGen
      | gen == 0 = newGen
      | otherwise =
          executeRules'
            (Bits.shiftR gen 1)
            (i+1)
            (newGen Bits..|.
              Bits.shiftL (lookupSpreadOutcome rules' gen) i)
    generation' = generation plantation
    rules' = rules plantation
    zeroIndexPos' = zeroIndexPos plantation

lookupSpreadOutcome :: Pots -> Integer -> Pot
lookupSpreadOutcome rules' gen =
  toInteger $
    rules' V.! (fromInteger (gen Bits..&. mask))
  where
    mask = 31 -- last five bits on

addPotsToEnd :: Plantation -> Plantation
addPotsToEnd plantation =
  case L.find (Bits.testBit generation') [0..3] of
    Just n -> updatePlantation n
    Nothing -> plantation
  where
    generation' = generation plantation
    updatePlantation n =
      plantation
        { generation = Bits.shiftL (generation plantation) (4-n)
        , zeroIndexPos = (zeroIndexPos plantation) + (4-n)
        }

removeRedundantPots :: Plantation -> Plantation
removeRedundantPots plantation
  | generation' /= 0 && (generation' Bits..&. endingMask == 0) =
      removeRedundantPots $
        plantation
          { generation =
              Bits.shiftR generation' 1
          , zeroIndexPos = zeroIndexPos' - 1
          }
  | otherwise = plantation
  where
    endingMask = 31 -- five last bits on
    generation' = generation plantation
    zeroIndexPos' = zeroIndexPos plantation

sumPlantsIndexes :: Plantation -> Integer
sumPlantsIndexes plantation =
  calculate generation' 0 0
  where
    calculate gen i result
      | gen == 0 = result
      | gen Bits..&. 1 == 1 =
          calculate
            (Bits.shiftR gen 1)
            (i+1)
            (result + ((toInteger zeroIndexPos') - i))
      | gen Bits..&. 1 == 0 =
          calculate
            (Bits.shiftR gen 1)
            (i+1)
            result
    generation' = generation plantation
    zeroIndexPos' = zeroIndexPos plantation

-- given pots and rules, what is the sum of indexes of all pots with plants?
solve :: Int -> String -> Integer
solve generationsN =
    sumPlantsIndexes .
    simulate generationsN .
    parseInput

solveB :: Int -> String -> Integer
solveB generationsN input =
  oneThousandSumsDelta
  * (numberOfThousandsInGenerationsN - 1)
  + sumPlantsIndexes oneThousandPlantation
  where
    numberOfThousandsInGenerationsN =
      toInteger $
        generationsN `div` oneThousand
    oneThousandSumsDelta =
      ((-) `F.on` sumPlantsIndexes)
        twoThousandPlantation
        oneThousandPlantation
    twoThousandPlantation =
      simulate oneThousand oneThousandPlantation
    oneThousandPlantation =
      simulate oneThousand .
      parseInput $
      input
    oneThousand = 1000
