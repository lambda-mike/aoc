module Day21 where

import qualified Data.Vector as V
import Day21.Day21 as Day21

main :: IO ()
main = do
  -- 16457176
  putStrLn "Solving Day21A..."
  print $
    Day21.solveA Day21.input

  -- 13625951
  -- 18m51.083s
  putStrLn "Solving Day21B..."
  print $
    Day21.solveB Day21.input
