module Day22 where

import Day22.Day22 as Day22

main :: IO ()
main = do

  -- 7299
  putStrLn "Solving Day22A..."
  print $
    Day22.solveA Day22.input

  -- 1008
  -- 11m38.277s
  putStrLn "Solving Day22B..."
  print $
    Day22.solveB input
