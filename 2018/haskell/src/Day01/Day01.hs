module Day01.Day01 where

import qualified Data.List as L
import qualified Data.Map.Strict as Map

solveA :: String -> Int
solveA =
  calibrate
  . L.map parseInt
  . lines

solveB :: String -> Int
solveB =
  findFirstDuplicate
  . calculateFrequencies
  . cycle
  . L.map parseInt
  . lines

calibrate :: [Int] -> Int
calibrate = sum

parseInt :: String -> Int
parseInt ('+':xs) = read xs
parseInt n = read n

calculateFrequencies :: [Int] -> [Int]
calculateFrequencies = L.scanl (+) 0

findFirstDuplicate :: [Int] -> Int
findFirstDuplicate frequencies =
  findDuplicate Map.empty frequencies
  where
    findDuplicate dict (f : fs) =
      if Map.lookup f dict /= Nothing then
        f
      else
        findDuplicate (Map.insert f f dict) fs

olfindFirstDuplicate :: [Int] -> Int
olfindFirstDuplicate frequencies =
  findDuplicate 2 frequencies
  where
    findDuplicate len frequencies =
      let
        freqsWindow = L.take len frequencies
      in
        if L.length freqsWindow /= (L.length $ L.nub freqsWindow) then
          L.head $ reverse freqsWindow
        else
          findDuplicate (len + 1) frequencies

oldfindFirstDuplicate :: [Int] -> Int
oldfindFirstDuplicate frequencies =
  findDuplicate 2 frequencies
  where
    findDuplicate len frequencies =
      let
        freqsWindow = reverse $ L.take len frequencies
      in
        if (L.head freqsWindow) `elem` (L.tail freqsWindow) then
          L.head freqsWindow
        else
          findDuplicate (len + 1) frequencies
