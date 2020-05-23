module Day14.Day14B
  ( solve
  ) where

import Data.Char (digitToInt)
import Data.Function ((&))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Day14.Day14Core

solve
  :: [Score]
  -> String
  -- ^ searched sequence of scores
  -> Int
  -- ^ number of scores before searched sequence
solve
  inputRecipes
  sequence
  =
    findRound
      sequenceRev
      (length sequenceRev)
      (length inputRecipes)
      inputRecipesRev
      scoresMap
      elvesPos
  where
    sequenceRev = map digitToInt . reverse $ sequence
    inputRecipesRev = reverse inputRecipes
    scoresMap = Map.fromList (zip [0..] inputRecipes)
    elvesPos = (0, 1)

findRound
  :: [Score]
  -> Int
  -> Int -- or Integer??
  -> [Score]
  -> Map Int Score
  -> Pos
  -> Int
findRound
  sequenceRev
  sequenceRevLen
  recipesCounter
  recipesRev
  scoresMap
  pos =
    if tail recipesRev == sequenceRev
    then recipesCounter - sequenceRevLen - 1
    else if init recipesRev == sequenceRev
         then recipesCounter - sequenceRevLen
         else findRound
                sequenceRev
                sequenceRevLen
                newRecipesCounter
                newRecipesRev
                newRecipesMap
                newPos
    where
      (newPos, newRecipesMap, newRecipesRev, newRecipesCounter) =
        calculateNextRound
          pos
          sequenceRevLen
          scoresMap
          recipesRev
          recipesCounter

calculateNextRound
  :: Pos
  -> Int
  -> Map Int Score
  -> [Score]
  -> Int
  -> (Pos, Map Int Score, [Score], Int)
  --  ^ elves pos, scores map, newRecipesRev, recipes num
calculateNextRound
  (elf1, elf2)
  seqLen
  scoresMap
  recipesRev
  recipesLen
    = ((newElf1, newElf2), newScoresMap, newRecipesRev, newRecipesLen)
    where
      newElf1 = (elf1 + 1 + elf1Score) `mod` newRecipesLen
      newElf2 = (elf2 + 1 + elf2Score) `mod` newRecipesLen
      elf1Score = scoresMap Map.! elf1
      elf2Score = scoresMap Map.! elf2
      newScoresDigits =
        getDigits
          (elf1Score + elf2Score)
      newRecipesRev =
        case newScoresDigits of
          [s] ->
            take (seqLen+1) (s : recipesRev)
          [s1, s2] ->
            take (seqLen+1) (s2 : s1 : recipesRev)
          -- fail otherwise
      newScoresMap =
        case newScoresDigits of
          [s] -> Map.insert recipesLen s scoresMap
          [s1, s2] -> Map.insert (recipesLen + 1) s2 $
                        Map.insert recipesLen s1 scoresMap
          -- fail otherwise
      newRecipesLen =
        length newScoresDigits + recipesLen

-- Assumption:
-- input nums always 0 <= x <= 9
getDigits
  :: Int
  -> [Int]
getDigits num =
  if num > 9
  then [num `div` 10, num `mod` 10]
  else [num]

