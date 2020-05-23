module Day17 where

import Day17.Day17 as Day17

-- A: 42389
-- B: 34497
main :: IO ()
main = do

  input <-
    readFile "src/Day17/input.txt"
  let source = [((500,0), Down)]

  putStrLn "Solving Day17A..."
  print $ Day17.solve input source

  putStrLn "Solving Day17B..."
  print $ Day17.solveB input source
