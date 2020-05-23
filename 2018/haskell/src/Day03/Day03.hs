module Day03.Day03 where

import qualified Data.List as L
import qualified Data.Map.Strict as Map

type Pos = (Int, Int)

data Claim =
  Claim
    { position :: Pos
    , ids :: [Int]
    }
    deriving Show

solveA :: String -> Int
solveA =
  calculateOverlapingArea
  . L.concat
  . L.map parseClaim
  . lines

solveB :: String -> Int
solveB =
  findNonOverlappingClaim
  . L.concat
  . L.map parseClaim
  . lines

split :: Char -> String -> [String]
split sep s =
  case dropWhile (== sep) s of
    "" -> []
    s' -> w : split sep s''
      where
        (w, s'') = break (== sep) s'

parseNumsPair :: Char -> String -> (Int, Int)
parseNumsPair separator str =
  let
    [x, y] =
      L.map (read :: String->Int)
      . split separator
      $ str
  in
    (x, y)

calculateOverlapingArea :: [Claim] -> Int
calculateOverlapingArea =
  Map.size
  . Map.filter ((> 1) . L.length . ids)
  . getClaimsDict

parseClaim :: String -> [Claim]
parseClaim c =
  let
    [idStr, _, startStr, sizeStr] = words c
    id' = (read :: String -> Int) $ tail idStr
    (x0, y0) = parseNumsPair ',' . init $ startStr
    (w, h) = parseNumsPair 'x' sizeStr
  in
    [ Claim
        { position = (x, y)
        , ids = [id']
        }
        | x <- [x0+1 .. x0+w]
        , y <- [y0+1 .. y0+h]
    ]

getClaimsDict :: [Claim] -> Map.Map Pos Claim
getClaimsDict =
  Map.fromListWith mergeClaims
  . L.map (\c -> (position c, c))

mergeClaims :: Claim -> Claim -> Claim
mergeClaims c1 c2 =
  Claim
    { position = pos
    , ids = L.union ids1 ids2
    }
  where
    Claim { position = pos, ids = ids1 } = c1
    Claim { ids = ids2 } = c2

findNonOverlappingClaim :: [Claim] -> Int
findNonOverlappingClaim claims =
  let
    summedClaims =
      getClaimsDict claims
    countIds =
      length . ids
    (singleClaims, overlappingClaims) =
      Map.partition ((<= 1) . countIds) summedClaims
    overlappingClaimsIds =
      L.nub
      . concat
      . map ids
      . Map.elems
      $ overlappingClaims
  in
    head
    . ids
    . head
    . filter (not . idOverlaps overlappingClaimsIds . head . ids)
    $ Map.elems singleClaims
  where
    idOverlaps overlappingIds id' =
      L.any ((==) id') overlappingIds

claim = "#1 @ 1,3: 4x4"

sample = "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2"
