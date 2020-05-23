module Day14B where

import Data.Vector.Unboxed as V

import Day14.Day14B

--
main :: IO ()
main = do
  putStrLn "Solving Day14B..."
  print $
    solve input sequence
  where
    input = scores
    scores = [3, 7]
    sequence = "170641"
    --sequence = "59414"
