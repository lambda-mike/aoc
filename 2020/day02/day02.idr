module Main

import Data.Bool.Xor
import Data.List
import Data.Maybe
import Data.Strings
import System.File

record Policy where
  constructor MkPolicy
  low, high: Int
  letter : Char

Show Policy where
  show (MkPolicy low high letter) =
    "Policy(" ++ show low ++ "," ++ show high ++ "," ++ show letter ++ ")"

readPasswordsFile : String -> IO String
readPasswordsFile file = do
  input <- readFile file
  pure $ either (const "") id input

parseNum : String -> Int
parseNum =
  fromMaybe (Delay 0) . parseInteger

-- This fn assumes the input is always perfectly formatted
parsePolicy : String -> (Policy, String)
parsePolicy lineStr =
      -- "1", "-3 a: abcde"
  let (lowNumStr, afterLow) =
        Data.Strings.span isDigit lineStr
      -- "-", "3 a: abcde"
      (_, afterLow2) =
        Data.Strings.break isDigit $ afterLow
      -- "3", " a: abcde"
      (highNumStr, afterHigh) =
        Data.Strings.span isDigit $ afterLow2
      -- "a", ": abcde"
      (letterStr, afterHigh2) =
        Data.Strings.break (== ':') $ trim afterHigh
      letter = fromMaybe (Delay ' ') . head' . unpack $ letterStr
      -- ":", " abcde"
      (_, passwordStr) = Data.Strings.span (== ':') afterHigh2
      -- " abcde", " abcde"
      password = trim passwordStr
      low = parseNum lowNumStr
      high = parseNum highNumStr
  in (MkPolicy low high letter, password)

verifyPasswordA : Policy -> String -> Bool
verifyPasswordA policy password =
  let letterFreq =
    the Int . cast . length . filter (== policy.letter) . unpack $ password
  in (letterFreq >= policy.low) && (letterFreq <= policy.high)

getNthLetterMaybe : Int -> String -> Maybe Char
getNthLetterMaybe n =
  let nat = fromInteger (the Integer (cast n))
  in head' . reverse . take nat . unpack

verifyPasswordB : Policy -> String -> Bool
verifyPasswordB policy password =
  let lowLetter = getNthLetterMaybe policy.low password
      highLetter = getNthLetterMaybe policy.high password
      policyLetter = Just policy.letter
  in xor (policyLetter == lowLetter) (policyLetter == highLetter)

||| Given policyChecker count number of valid passwords
solve : (Policy -> String -> Bool) -> String -> Int
solve verifyPassword
  = the Int
  . cast
  . length
  . filter (uncurry verifyPassword)
  . map parsePolicy
  . lines

solveA : String -> Int
solveA = solve verifyPasswordA

solveB : String -> Int
solveB = solve verifyPasswordB

mainA : String -> IO Int
mainA file = do
  passwordsStr <- readPasswordsFile file
  pure $ solveA passwordsStr

mainB : String -> IO Int
mainB file = do
  passwordsStr <- readPasswordsFile file
  pure $ solveB passwordsStr

main : IO ()
main = do
  let file = "input.txt"
  -- 628
  putStrLn "Solving Day02A..."
  a <- mainA file
  putStrLn $ show a
  -- 705
  putStrLn "Solving Day02B..."
  b <- mainB file
  putStrLn $ show b

-- TESTS

testParsePolicy : IO ()
testParsePolicy = do
  putStrLn $ show $ parsePolicy "1-3 a: abcde"
  putStrLn $ show $ parsePolicy "13-15 n: nznnnncnnnnnnnf"

testVerifyPwdA : IO ()
testVerifyPwdA = do
  putStrLn . show . verifyPasswordA (MkPolicy 1 3 'a') $ "abcde"
  putStrLn . show . verifyPasswordA (MkPolicy 1 3 'b') $ "cdefg"

testVerifyPwdB : IO ()
testVerifyPwdB = do
  putStrLn . show . verifyPasswordB (MkPolicy 1 3 'a') $ "abcde"
  putStrLn . show . verifyPasswordB (MkPolicy 1 3 'b') $ "cdefg"
  putStrLn . show . verifyPasswordB (MkPolicy 2 9 'c') $ "ccccccccc"

tests : IO ()
tests = do
  testParsePolicy
  putStrLn ""
  testVerifyPwdA
  putStrLn ""
  testVerifyPwdB
