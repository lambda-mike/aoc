module Day14.Day14Core
  ( Input
  , Pos
  , Score
  ) where

import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed as V

-- Recipe score from 0 to 9
type Score = Int

-- Pos is a position of both Elves
-- in the list of scores
type Pos = (Int, Int)

type Input = ([Score], Pos)

