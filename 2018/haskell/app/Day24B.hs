module Day24B where

import Day24.Day24

main :: IO ()
main = do
  input <- readFile "src/Day24/input.txt"
  let armies =
        readArmies input
  putStrLn "Solving Day24B..."
  print $
    solveB armies

