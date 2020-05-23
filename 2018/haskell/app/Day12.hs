module Day12 where

import Day12.Day12 as Day12

main :: IO ()
main = do
  input <-
    readFile "src/Day12/input.txt"
  -- 3221
  -- 0.4s
  -- Integer & bits: 0.386s
  putStrLn "Solving Day12A..."
  print $
    Day12.solve Day12.genNumA input

  -- 2600000001872
  putStrLn "Solving Day12B..."
  print $
    Day12.solveB Day12.genNumB input

