module Day18 where

import Day18.Day18 as Day18

-- 0.42s
-- 0.87s
main :: IO ()
main = do
  input <-
    readFile "src/Day18/input.txt"
  -- 495236
  putStrLn "Solving Day18A..."
  print $
    Day18.solve Day18.iterationsA input

  -- 201348
  putStrLn "Solving Day18B..."
  print $
    Day18.solveB Day18.iterationsB input
