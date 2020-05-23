module Day16 where

import Day16.Day16 as Day16

main :: IO ()
main = do

  inputA <- readFile "src/Day16/inputA.txt"
  inputB <- readFile "src/Day16/inputB.txt"

  -- 596
  putStrLn "Solving Day16A..."
  print $
    Day16.solveA inputA

  -- 554
  putStrLn "Solving Day16B..."
  print $
    Day16.solveB $
      Day16.splitInput inputB
