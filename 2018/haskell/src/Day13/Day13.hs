module Day13.Day13 where

import Data.List
import qualified Data.Vector as V


type Pos = (Int, Int)

data Track
    = Empty
    | Vertical
    | Horizontal
    | Cross
    | Slash
    | Backslash deriving (Eq, Show)

type TracksMap = V.Vector (V.Vector Track)

-- Up, Down, Left, Right
data Dir = U | D | L | R deriving (Eq, Show)

-- TurnLeft, GoStraight, TurnRight
data NextTurn = TL | GS | TR deriving (Eq, Show)

data Cart = Cart {
    pos :: Pos,
    dir :: Dir,
    nextTurn :: NextTurn
} deriving (Eq, Show)

data Cell = T Track | C Cart


solveA :: String -> Pos
solveA input =
  let
    response =
      locateCrush
      .  parseTracksMap
      $ input
  -- col is X, row is Y
  in (snd response, fst response)

solveB :: String -> Pos
solveB input =
  let response =
        findLastSurvivor
        .  parseTracksMap
        $ input
  -- col is X, row is Y
  in (snd response, fst response)


parseCell :: Pos -> Char -> Cell
parseCell _ ' '  = T Empty
parseCell _ '|'  = T Vertical
parseCell _ '-'  = T Horizontal
parseCell _ '+'  = T Cross
parseCell _ '/'  = T Slash
parseCell _ '\\' = T Backslash
parseCell p '^'  = C Cart { pos = p, dir = U, nextTurn = TL }
parseCell p 'v'  = C Cart { pos = p, dir = D, nextTurn = TL }
parseCell p '<'  = C Cart { pos = p, dir = L, nextTurn = TL }
parseCell p '>'  = C Cart { pos = p, dir = R, nextTurn = TL }

sortByPos :: [Cart] -> [Cart]
sortByPos cs =
  Data.List.sortBy comparePositions cs
  where
    comparePositions c1 c2 =
      compare (pos c1) (pos c2)

mergeCells :: [[Cell]] -> (TracksMap, [Cart])
mergeCells cells =
  let (tmap, carts) = foldl mergeRows (V.empty, []) cells
  in  (tmap, sortByPos carts)

mergeRows :: (V.Vector (V.Vector Track), [Cart]) -> [Cell] -> (V.Vector (V.Vector Track), [Cart])
mergeRows (rows, carts) cells =
  let
    (row, rowCarts) =
      foldl mergeCols (V.empty, []) cells
  in
    (addNewRow rows row, carts ++ rowCarts)
  where
    addNewRow = V.snoc

mergeCols :: (V.Vector Track, [Cart]) -> Cell -> (V.Vector Track, [Cart])
mergeCols (tracks, carts) (T track) = (V.snoc tracks track, carts)
mergeCols (tracks, carts) (C cart)  = addCart tracks carts cart

addCart :: V.Vector Track -> [Cart] -> Cart -> (V.Vector Track, [Cart])
addCart tracks carts cart
  | dir cart == U || dir cart == D = (V.snoc tracks Vertical,  cart : carts)
  | dir cart == L || dir cart == R = (V.snoc tracks Horizontal, cart : carts)

parseMapLine :: (Int, String) -> [Cell]
parseMapLine (row, line) =
  map (\(col, c) -> parseCell (row, col) c) $
  zip [0..] line

parseTracksMap :: String -> (TracksMap, [Cart])
parseTracksMap =
  mergeCells .
  map parseMapLine .
  zip [0..] .
  lines

locateCrush :: (TracksMap, [Cart]) -> Pos
locateCrush (tracks, carts) =
  tick tracks carts Nothing

tick :: TracksMap -> [Cart] -> Maybe Pos -> Pos
tick tracks carts crush =
  case crush of
    Just pos -> pos
    Nothing ->
      let (newCarts, newCrush) = moveCarts tracks (sortByPos carts)
      in  tick tracks newCarts newCrush

moveCarts :: TracksMap -> [Cart] -> ([Cart], Maybe Pos)
moveCarts tracks carts =
  let
    (_, newCarts, newCrush) =
      foldl (moveCart tracks) (carts, [], Nothing) carts
  in
    (newCarts, newCrush)

moveCart :: TracksMap -> ([Cart], [Cart], Maybe Pos) -> Cart -> ([Cart], [Cart], Maybe Pos)
moveCart tracks (waitingCarts, movedCarts, crushPos) cart@Cart{ pos = posVal, dir = dirVal, nextTurn = ntVal } =
  let
      nextPos = getNextPos posVal dirVal
      nextTrack = getTrack tracks nextPos
      nextDir = getNextDir nextTrack ntVal dirVal
      newNextTurn = getNewNextTurn nextTrack ntVal
      movedCart = Cart { pos = nextPos, dir = nextDir, nextTurn = newNextTurn }
      otherWaitingCarts = filter (\c -> c /= cart) waitingCarts
      newCrushPos = pos <$> (find (isCrush movedCart) (otherWaitingCarts ++ movedCarts))
  in
      (otherWaitingCarts, movedCart : movedCarts, updateCrushPos crushPos newCrushPos)

updateCrushPos :: Maybe Pos -> Maybe Pos -> Maybe Pos
updateCrushPos crush newCrush =
  case crush of
    Nothing -> newCrush
    Just _  -> crush

getNextPos :: Pos -> Dir -> Pos
getNextPos (row,col) dir
  | dir == U = (row - 1, col    )
  | dir == D = (row + 1, col    )
  | dir == L = (row    , col - 1)
  | dir == R = (row    , col + 1)

getTrack :: TracksMap -> Pos -> Track
getTrack tracks (row, col) =
  tracks V.! row V.! col

getNextDir :: Track -> NextTurn -> Dir -> Dir
getNextDir track turn dir
  | track == Vertical   = dir
  | track == Horizontal = dir
  | track == Cross      = cross dir turn
  | track == Slash      = curve dir track
  | track == Backslash  = curve dir track

cross :: Dir -> NextTurn -> Dir
cross dir nt
  | nt == TL = changeDirLeft dir
  | nt == GS = dir
  | nt == TR = changeDirRight dir

changeDirLeft :: Dir -> Dir
changeDirLeft U = L
changeDirLeft D = R
changeDirLeft L = D
changeDirLeft R = U

changeDirRight :: Dir -> Dir
changeDirRight U = R
changeDirRight D = L
changeDirRight L = U
changeDirRight R = D

curve :: Dir -> Track -> Dir
curve U Slash     = R
curve U Backslash = L
curve D Slash     = L
curve D Backslash = R
curve L Slash     = D
curve L Backslash = U
curve R Slash     = U
curve R Backslash = D

getNewNextTurn :: Track -> NextTurn -> NextTurn
getNewNextTurn Cross TL = GS
getNewNextTurn Cross GS = TR
getNewNextTurn Cross TR = TL
getNewNextTurn _     nt = nt

isCrush :: Cart -> Cart -> Bool
isCrush c1 c2 =
  (pos c1) == (pos c2)

findLastSurvivor :: (TracksMap, [Cart]) -> Pos
findLastSurvivor (tracks, carts) =
  tickB tracks carts

tickB :: TracksMap -> [Cart] -> Pos
tickB tracks carts =
  case carts of
    [ c ] ->
      -- pos $ head $ moveCarts tracks carts
      pos c
    _ ->
      tickB tracks
        $ moveCartsB tracks (sortByPos carts)

moveCartsB :: TracksMap -> [Cart] -> [Cart]
moveCartsB tracks carts =
  let
    (_, newCarts) =
        foldl (moveCartB tracks) (carts, []) carts
  in
    newCarts

moveCartB
  :: TracksMap
  -> ([Cart], [Cart])
  -> Cart
  -> ([Cart], [Cart])
moveCartB tracks (waitingCarts, movedCarts) cart@Cart{ pos = posVal, dir = dirVal, nextTurn = ntVal } =
  if elem cart waitingCarts then
    let nextPos = getNextPos posVal dirVal
        nextTrack = getTrack tracks nextPos
        nextDir = getNextDir nextTrack ntVal dirVal
        newNextTurn = getNewNextTurn nextTrack ntVal
        movedCart = Cart { pos = nextPos, dir = nextDir, nextTurn = newNextTurn }
        otherWaitingCarts = filter (\c -> c /= cart) waitingCarts
        otherCrushedCart = find (isCrush movedCart) otherWaitingCarts
        otherMovedCrushedCart = find (isCrush movedCart) movedCarts
    in
        (excludeCrushedCart otherWaitingCarts otherCrushedCart,
         includeNotCrushedCart movedCart otherCrushedCart otherMovedCrushedCart $
          excludeCrushedCart movedCarts otherMovedCrushedCart)
else
  (waitingCarts, movedCarts)

excludeCrushedCart :: [Cart] -> Maybe Cart -> [Cart]
excludeCrushedCart carts Nothing  = carts
excludeCrushedCart carts (Just c) = filter ((/=) c) carts

includeNotCrushedCart :: Cart -> Maybe Cart -> Maybe Cart -> [Cart] -> [Cart]
includeNotCrushedCart cart crushed1 crushed2 carts
  | crushed1 == Nothing && crushed2 == Nothing = cart : carts
  | otherwise                                  = carts
