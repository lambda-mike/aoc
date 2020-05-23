module Day25 where

import Day25.Day25 as Day25

main :: IO ()
main = do

  input <- readFile "src/Day25/input.txt"

  -- 399
  putStrLn "Solving Day25A..."
  print $
    Day25.solve input
