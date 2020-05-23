module Day14A where

import Data.Vector.Unboxed as V

import Day14.Day14A

-- 2103141159
main :: IO ()
main = do
  putStrLn "Solving Day14A..."
  putStrLn $
    printSolution $
      solve input scoresNum ignoredNum
  where
    input = scores
    scores = V.fromList [3, 7]
    scoresNum = 10
    ignoredNum = 170641 -- Day14A input
    --ignoredNum = 2018
    printSolution =
      Prelude.concatMap show
