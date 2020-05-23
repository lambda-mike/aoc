module Day14.Day14A
  ( solve
  ) where

import Data.Function ((&))
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed as V

import Day14.Day14Core

solve
  :: Vector Score
  -> Int
  -- ^ number of recipes to get (10 by default)
  -> Int
  -- ^ number of recipes to throw away
  -> [Score]
  -- ^ list of scores
solve
  scores
  recipesNum
  ignoredNum =
    findRound recipesNum ignoredNum (scores, elvesPositions)
      & fst
      & V.drop ignoredNum
      & V.take recipesNum
      & V.toList
  where
    elvesPositions = (0, 1)

findRound
  :: Int
  -> Int
  -> (Vector Score, Pos)
  -> (Vector Score, Pos)
findRound recipesNum ignoredNum (scores, positions) =

  findRound' (scores, positions)

  where

    findRound' input@(scores, positions) =
      if V.length scores >= requiredRecipiesNum
      then input
      else findRound' (calculateNextRound input)

    requiredRecipiesNum = recipesNum + ignoredNum

calculateNextRound
  :: (Vector Score, Pos)
  -> (Vector Score, Pos)
calculateNextRound
  (scores, (elf1, elf2))
    = (newScores, (newElf1, newElf2))
    where
      newScores =
        scores V.++
          (getDigits
            (elf1Score +
             elf2Score))
      elf1Score = scores V.! elf1
      elf2Score = scores V.! elf2
      newElf1 = (elf1 + 1 + elf1Score) `mod` (V.length newScores)
      newElf2 = (elf2 + 1 + elf2Score) `mod` (V.length newScores)

-- Assumption:
-- input nums always 0 <= x <= 9
getDigits
  :: Int
  -> Vector Int
getDigits num =
  if num > 9
  then V.fromList [num `div` 10, num `mod` 10]
  else V.singleton num

