module Day09 where

import Day09.Day09 as Day09

main :: IO ()
main = do

  let players = 479
      lastMarbleA = 71035
      lastMarbleB = lastMarbleA * 100

  -- 367634
  -- Boxed Vector: 37.577s
  -- Unboxed Vector: 2.931s
  -- Lazy lists: 1.184s
  putStrLn "Solving Day09A..."
  print $
    Day09.solve players lastMarbleA

  -- 3020072891
  -- Unboxed Vector: ~20h
  -- Lazy lists: 13.601s
  putStrLn "Solving Day09B..."
  print $
    Day09.solve players lastMarbleB
