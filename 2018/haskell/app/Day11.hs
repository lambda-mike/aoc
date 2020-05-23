module Day11 where

import Day11.Day11 as Day11

main :: IO ()
main = do
  -- (34,72)
  -- 1.243s old
  -- 2.203s new
  putStrLn "Solving Day11A..."
  print $
    Day11.solveA
      Day11.width
      Day11.height
      Day11.serialNumber

  -- ((233,187,13),91)
  -- 9m17.507s
  -- 5m48.867s
  putStrLn "Solving Day11B..."
  print $
    Day11.solveB
      Day11.width
      Day11.height
      Day11.serialNumber

