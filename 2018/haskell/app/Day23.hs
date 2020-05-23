module Day23 where

import Day23.Day23 as Day23

main :: IO ()
main = do
  input <-
    readFile "src/Day23/input.txt"

  -- 935
  putStrLn "Solving Day23A..."
  print $ Day23.solveA input

  -- 138697281
  -- 15m39.318s
  putStrLn "Solving Day23B..."
  print $ Day23.solveB input

