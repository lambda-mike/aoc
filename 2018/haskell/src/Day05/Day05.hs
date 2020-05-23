module Day05.Day05 where

import qualified Data.List as L
import Data.Char

solveA :: String -> Int
solveA =
  L.length
  . triggerReactions
  . L.head
  . lines

solveB :: String -> Int
solveB input =
  let
    polymer =
      L.head . L.lines $ input
    getLengthWithoutType =
        L.length
        . triggerReactions
        . excludeType polymer
  in
    L.foldl
      (\minSoFar ->
        min minSoFar . getLengthWithoutType)
      (maxBound::Int)
      listOfTypes

triggerReactions :: String -> String
triggerReactions input =
  L.reverse $ react input []

react :: String -> String -> String
react []     []            = []
react []     reacted       = reacted
react (p:ps) []            = react ps [p]
react (p:ps) (r:rs)
  | oppositePolarity p r   = react ps rs
  | otherwise              = react ps (p:r:rs)

oppositePolarity :: Char -> Char -> Bool
oppositePolarity c1 c2 =
  dist == matchDistance
  where
    dist =
      abs $ c1Int - c2Int
    matchDistance =
      ord 'a' - ord 'A'
    c1Int =
      ord c1
    c2Int =
      ord c2

excludeType :: String -> (Char, Char) -> String
excludeType polymer (lowercaseT, uppercaseT) =
  L.filter
    (\c -> c /= lowercaseT && c /= uppercaseT)
    polymer

listOfTypes :: [(Char, Char)]
listOfTypes =
  L.zip ['a'..'z'] ['A'..'Z']
