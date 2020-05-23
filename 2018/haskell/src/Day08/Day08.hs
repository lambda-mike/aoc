module Day08.Day08 where

import qualified Data.List as L
import qualified Data.Map as Map

solveA :: String -> Int
solveA =
  sumMetadata
  . L.map (read :: String -> Int)
  . words

solveB :: String -> Int
solveB =
  sumMetadataB
  . L.map (read :: String -> Int)
  . words

sumMetadata :: [Int] -> Int
sumMetadata input =
  let
    (_, total) = handleNode input 0
  in
    total

handleNode :: [Int] -> Int -> ([Int], Int)
handleNode input total
  | input == []      = (input, total)
  | childrenN == 0   =
      ( drop metadataN rest
      , total + calculateMetadataSum metadataN rest
      )
  | otherwise        =
      let
        (newRest, newTotal) =
          L.foldr
            (\_ -> uncurry handleNode)
            (rest, total)
            [1..childrenN]
    in
      ( drop metadataN newRest
      , newTotal + calculateMetadataSum metadataN newRest
      )
  where
    childrenN : metadataN : rest = input
    calculateMetadataSum n =
      L.sum . L.take n

sumMetadataB :: [Int] -> Int
sumMetadataB input =
    let
        (_, total) = handleNodeB input
    in
        total

handleNodeB :: [Int] -> ([Int], Int)
handleNodeB input
    | childrenN == 0   =
        (drop metadataN rest, calculateMetadataSum metadataN rest)
    | otherwise        =
        let
            (newRest, children) =
                foldl addToChildrenDict (rest, Map.empty) [1..childrenN]
        in
            (drop metadataN newRest, sumChildren children $ take metadataN newRest)
    where
        childrenN : metadataN : rest = input
        calculateMetadataSum n =
            sum . take n
        addToChildrenDict (rest, dict) n =
            let (newRest, value) = handleNodeB rest
            in  (newRest, Map.insert n value dict)
        sumChildren dict keys =
            foldr (\i s -> s + Map.findWithDefault 0 i dict) 0 keys

sample = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
