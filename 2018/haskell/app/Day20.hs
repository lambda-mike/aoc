{-# LANGUAGE OverloadedStrings #-}

module Day20 where

import qualified Data.Text as T
import Day20.Day20 as Day20

main :: IO ()
main = do
  input <-
    T.pack
    <$> readFile "src/Day20/input.txt"

  -- 3721
  putStrLn "Solving Day20A..."
  print $
    Day20.solve input

  -- 8613
  putStrLn "Solving Day20B..."
  print $
    Day20.solve2 input

