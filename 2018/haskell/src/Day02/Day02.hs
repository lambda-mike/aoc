module Day02.Day02 where

import qualified Data.Map.Strict as Map

type Times = (Int, Int)

solveA :: String -> Int
solveA =
  calculateChecksum
  .  foldr (sumOccurances . countLettersOccurance) (0, 0)
  .  lines

solveB :: String -> String
solveB =
  findCommonLetters . lines

calculateChecksum :: Times -> Int
calculateChecksum (a, b) = a * b

countLettersOccurance :: String -> Times
countLettersOccurance s =
  let
    occurancesMap =
      Map.filter (\v -> v == 2 || v == 3) $
      Map.fromListWith (+) $
      map (\c -> (c, 1)) s
    twoes =
      Map.filter ((==) 2) occurancesMap
    threes =
      Map.filter ((==) 3) occurancesMap
  in
    mapsToTimes (twoes, threes)

mapsToTimes :: (Map.Map Char Int, Map.Map Char Int) -> Times
mapsToTimes (m2, m3)
  | m2 == Map.empty && m3 == Map.empty = (0, 0)
  | m3 == Map.empty                    = (1, 0)
  | m2 == Map.empty                    = (0, 1)
  | otherwise                          = (1, 1)

sumOccurances :: Times -> Times -> Times
sumOccurances (x2, x3) (y2, y3) =
  (x2 + y2, x3 + y3)

-- find common letters for two correct boxes
findCommonLetters :: [String] -> String
findCommonLetters boxes =
  findCommonLetters' boxes boxes Nothing
  where
    findCommonLetters' (b:bs) boxes' result =
      case result of
        Just letters ->
          letters
        Nothing ->
          let
            newResult = findCommonLettersForGivenBox b boxes'
          in
            findCommonLetters' bs boxes' newResult

-- find common letters for given box and potentially one of other boxes
findCommonLettersForGivenBox :: String -> [String] -> Maybe String
findCommonLettersForGivenBox box (b:bs) =
  let
    result = compareBoxes box b
  in
    if result /= Nothing || bs == [] then
      result
    else
      findCommonLettersForGivenBox box bs

compareBoxes :: String -> String -> Maybe String
compareBoxes s1 s2 =
  compareBoxes' s1 s2 0 $ Just ""
  where
    compareBoxes' s1 s2 differencesCount result =
      if s1 == [] then
        if differencesCount == 1 then
          reverse <$> result
        else
          Nothing
      else
        let
          (c1:cs1) = s1
          (c2:cs2) = s2
        in
          if (c1 == c2) then
            compareBoxes' cs1 cs2 differencesCount
              $ (c1 :) <$> result
          else
            compareBoxes' cs1 cs2 (differencesCount + 1) result
