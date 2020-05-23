module Day19 where

import qualified Data.Vector as V
import Day19.Day19 as Day19

main :: IO ()
main = do

  let registersA = V.replicate 6 0
      registersB = 1 `V.cons` V.replicate 5 0

  input <-
    readFile "src/Day19/input.txt"

  -- 1248
  putStrLn "Solving Day19A..."
  print $
    Day19.solve registersA input

  -- 14952912
  putStrLn "Solving Day19B..."
  print $
    Day19.solveB registersB input
