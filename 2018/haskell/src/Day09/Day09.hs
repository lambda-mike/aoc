module Day09.Day09 where

import qualified Data.List as L
import qualified Data.Vector.Unboxed as V

data GameState = GameState
  { currentMarbleIndex :: !Int
  , marble             :: !Int
  , lastMarble         :: !Int
  , marbles            :: ![Int]
  , marblesNum         :: !Int
  , prevMarbles        :: ![Int]
  , prevMarblesNum     :: !Int
  , scoreTmpMarbles    :: ![Int]
  , playersScores      :: !(V.Vector Int)
  , player             :: !Int
  } deriving (Eq, Show)

scoreDivisor :: Int
scoreDivisor = 23

-- given number of players and score value of the last marble,
-- calculate what is the highest score
solve :: Int -> Int -> Int
solve playersNum lastMarble =
  findHighestScore $
  startGame
    playersNum
    lastMarble

startGame :: Int -> Int -> GameState
startGame playersNum lastMarble =
  play $ initialGameState playersNum lastMarble

initialGameState
  :: Int
  -> Int
  -> GameState
initialGameState playersNum lastMarble =
  GameState
    { currentMarbleIndex = 0
    , marble             = 1
    , lastMarble         = lastMarble
    , marbles            = [0]
    , marblesNum         = 1
    , prevMarbles        = []
    , prevMarblesNum     = 0
    , scoreTmpMarbles    = []
    , playersScores      = V.replicate playersNum 0
    , player             = 0
    }

play
  :: GameState
  -> GameState
play game
  | marble' > lastMarble' = game
  | otherwise =
    if isScoreRound marble'
    then play $ scoreMarble game marble'
    else play $ addMarble game marble'
  where
    marble' = marble game
    lastMarble' = lastMarble game

isScoreRound :: Int -> Bool
isScoreRound = (== 0) . (`mod` scoreDivisor)

scoreMarble :: GameState -> Int -> GameState
scoreMarble game marble'
  | secondScoreMarbleIndex < currentMarbleIndex' =
      let (beforeScoreMarble, secondScoreMarble : marblesRest) =
            L.splitAt 7 marbles'
          newCurrentMarble : newScoreTmpMarbles  =
            L.reverse beforeScoreMarble
          newScore =
            secondScoreMarble + marble'
          newPlayersScore =
            updatePlayersScore
              game
              newScore
      in (addNewGameBase newCurrentIndex game)
          { marblesNum =
              marblesNum' - 1
          , marbles =
              newCurrentMarble : marblesRest
          , scoreTmpMarbles =
              newScoreTmpMarbles
          , playersScores =
              newPlayersScore
          }
  -- Remove last marble
  | newCurrentIndex == 0 =
      let newCurrentMarble : newScoreTmpMarbles  =
            L.reverse marbles'
          secondScoreMarble =
            L.head $ L.reverse prevMarbles'
          newScore =
            secondScoreMarble + marble'
          newPlayersScore =
            updatePlayersScore
              game
              newScore
      in (addNewGameBase newCurrentIndex game)
          { marblesNum =
              marblesNum' - 1
          , marbles =
              [newCurrentMarble]
          , scoreTmpMarbles =
              newScoreTmpMarbles
          , prevMarblesNum =
              prevMarblesNum' - 1
          , playersScores =
              newPlayersScore
          }
  -- Remove last but one marble
  | newCurrentIndex == (marblesNum' - 1) =
      let newFirstMarble : newScoreTmpMarbles  =
            L.reverse marbles'
          prevLast : secondScoreMarble : prevRest =
            L.reverse prevMarbles'
          newScore =
            secondScoreMarble + marble'
          newPlayersScore =
            updatePlayersScore
              game
              newScore
      in (addNewGameBase newCurrentIndex game)
          { currentMarbleIndex = 1
          , marble = marble' + 2
          , marblesNum = marblesNum'
          , marbles =
              marble' + 1 : newFirstMarble : []
          -- Replace last but one with the last marble
          , prevMarbles =
              L.reverse $ prevLast : prevRest
          , prevMarblesNum =
              prevMarblesNum' - 1
          , scoreTmpMarbles =
              newScoreTmpMarbles
          , playersScores =
              newPlayersScore
          , player =
              getNextPlayer
                (getPlayersNum game)
                (player game + 1)
          }
  -- Remove marbles earlier than last but one
  | newCurrentIndex < (marblesNum' - 1) =
      let prevBeforeScoreMarbleNum =
            prevMarblesNum' - (7 - L.length marbles' + 1)
          (toNewMarbles,
            secondScoreMarble : newLastMarble : newPrev) =
              L.splitAt prevBeforeScoreMarbleNum prevMarbles'
          newScore =
            secondScoreMarble + marble'
          newPlayersScore =
            updatePlayersScore
              game
              newScore
      in (addNewGameBase newCurrentIndex game)
          { marblesNum =
              marblesNum' - 1
          , marbles =
              newLastMarble : (L.reverse toNewMarbles ++ marbles')
          , prevMarbles =
              newPrev
          , prevMarblesNum = L.length newPrev
          , playersScores =
              newPlayersScore
          }
  where
    currentMarbleIndex' =
      currentMarbleIndex game
    marbles' =
      marbles game
    marblesNum' =
      marblesNum game
    prevMarbles' =
      prevMarbles game
    prevMarblesNum' =
      prevMarblesNum game
    secondScoreMarbleIndex =
      getScoreMarbleIndex game
    newCurrentIndex =
      getNewCurrentMarbleIndex
        marblesNum'
        secondScoreMarbleIndex

addMarble :: GameState -> Int -> GameState
addMarble game marble
  | nextCurrentMarbleIndex > marblesNum' =
      startNewMarbleRow game marble
  | otherwise =
      addToExistingRow game nextCurrentMarbleIndex marble
  where
    marblesNum' =
      marblesNum game
    nextCurrentMarbleIndex =
      currentMarbleIndex game + 2

startNewMarbleRow :: GameState -> Int -> GameState
startNewMarbleRow game marble' =
  (addNewGameBase nextIndex game)
    { prevMarbles = L.tail newMarbles
    , prevMarblesNum = marblesNum' - 1
    , marbles =
        [ marble', L.head newMarbles ]
    }
  where
    nextIndex = 1
    newMarbles = L.reverse marbles'
    marbles' = marbles game
    marblesNum' = marblesNum game

addToExistingRow
  :: GameState
  -> Int
  -> Int
  -> GameState
addToExistingRow game nextIndex marble'
  | marble' == 1 =
      newGame
        { prevMarbles =
            [1]
        , prevMarblesNum = 1
        , marbles =
            [1, 0]
        }
  | scoreTmpMarbles' /= [] =
      addFromScoreTmpMarbles
        newGame
        nextIndex
        marble'
  | otherwise =
      addFromPrevMarbles
        newGame
        nextIndex
        marble'
  where
    newGame = addNewGameBase nextIndex game
    scoreTmpMarbles' = scoreTmpMarbles game

addFromScoreTmpMarbles
  :: GameState
  -> Int
  -> Int
  -> GameState
addFromScoreTmpMarbles
  game
  nextIndex
  marble' =
  game
    { scoreTmpMarbles = L.tail scoreTmpMarbles'
    , marbles =
        marble'
        : L.head scoreTmpMarbles'
        : marbles'
    }
  where
    scoreTmpMarbles' = scoreTmpMarbles game
    marbles' = marbles game

addFromPrevMarbles
  :: GameState
  -> Int
  -> Int
  -> GameState
addFromPrevMarbles
  game
  nextIndex
  marble' =
  game
    { prevMarbles = L.tail prevMarbles'
    , prevMarblesNum = prevMarblesNum' - 1
    , marbles =
        marble'
        : L.head prevMarbles'
        : marbles'
    }
  where
    marbles' = marbles game
    prevMarbles' = prevMarbles game
    prevMarblesNum' = prevMarblesNum game

addNewGameBase
  :: Int
  -> GameState
  -> GameState
addNewGameBase nextIndex game =
  game
    { currentMarbleIndex = nextIndex
    , marble = marble game + 1
    , marblesNum = marblesNum game + 1
    , player =
        getNextPlayer
          (getPlayersNum game)
          (player game)
    }

-- ?
getScoreMarbleIndex :: GameState -> Int
getScoreMarbleIndex game =
  let
    indexToRemove = currentMarbleIndex game - 7
  in
    if indexToRemove >= 0 then
      indexToRemove
    else
      -- move to the left starting from the end
      marblesNum game + indexToRemove

updatePlayersScore :: GameState -> Int -> V.Vector Int
updatePlayersScore game score =
  let scores     = playersScores game
      player'    = player game
      scoreSoFar = scores V.! player'
      newScore   = scoreSoFar + score
  in
      scores V.// [(player', newScore)]

getPlayersNum :: GameState -> Int
getPlayersNum =
  V.length . playersScores

getNextPlayer :: Int -> Int -> Int
getNextPlayer playersNum p =
  (p + 1) `mod` playersNum

getNewCurrentMarbleIndex
  :: Int
  -> Int
  -> Int
getNewCurrentMarbleIndex marblesNum' removedIndex =
  if removingLastMarble
  then firstMarbleIndex
  else removedIndex
  where
    removingLastMarble =
      removedIndex + 1 == marblesNum'
    firstMarbleIndex = 0

findHighestScore :: GameState -> Int
findHighestScore =
  V.maximum . playersScores

