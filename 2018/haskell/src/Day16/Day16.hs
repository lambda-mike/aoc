module Day16.Day16 where

import Control.Monad (join)
import Data.Bits
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type OpInput = (Int, Int, Int)
type Registers = [Int]

data InstructionTest =
  InstructionTest
    { opId :: Int
    , input :: OpInput
    , before :: Registers
    , after :: Registers
    }
    deriving (Eq, Show)

data Op
  = ADDR
  | ADDI
  | MULR
  | MULI
  | BANR
  | BANI
  | BORR
  | BORI
  | SETR
  | SETI
  | GTIR
  | GTRI
  | GTRR
  | EQIR
  | EQRI
  | EQRR
  deriving (Eq, Ord, Show)

operations :: [Op]
operations =
    [ ADDR, ADDI, MULR, MULI, BANR, BANI, BORR, BORI,
      SETR, SETI, GTIR, GTRI, GTRR, EQIR, EQRI, EQRR ]

solveA :: String -> Int
solveA =
  length
  . findAmbiguousSamples
  . parseInstructionsTests

solveB
  :: ([[String]], [String])
  -> Int
solveB (samples, instructions) =
  let opsCoding     = discoverOpsCoding
                        $ parseInstructionsTestsB samples
      instructions' = map parseInstruction instructions
  in
    (executeInstructions opsCoding instructions') !! 0

splitInput :: String -> ([[String]], [String])
splitInput =
  mergeStrings .
  L.span startsWithB .
  groupBy 3 [] .
  L.filter ((/=) "") .
  L.lines

startsWithB :: [String] -> Bool
startsWithB = (== 'B') . L.head . L.head

mergeStrings
  :: ([[String]], [[String]])
  -> ([[String]], [String])
mergeStrings (s1, s2) = (s1, concat s2)

executeInstructions
  :: Maybe (M.Map Int Op)
  -> [(Int, OpInput)]
  -> Registers
executeInstructions Nothing _ =
  error "Unknown coding occurred"
executeInstructions (Just opsCoding) instructions =
  let initialRegs = L.replicate 4 0
  in  L.foldl updateRegisters initialRegs instructions
  where
      getOp = (opsCoding M.!)
      updateRegisters regs (opId, input) =
        executeOp (getOp opId) input regs

discoverOpsCoding
  :: [InstructionTest]
  -> Maybe (M.Map Int Op)
discoverOpsCoding instructionTests =
  let operationsCandidates =
        L.sortOn (S.size . snd) .
        M.assocs $
        generateOperationsCandidatesHistogram instructionTests
      operationsTests =
        L.foldr insertSample M.empty instructionTests
  in
      discoverOpsCodingHelper operationsTests operationsCandidates

discoverOpsCodingHelper
  :: M.Map Int [InstructionTest]
  -> [(Int, (S.Set Op))]
  -> Maybe (M.Map Int Op)
discoverOpsCodingHelper operationsTests candidates =
  discoverOpsCodingRec operationsTests candidates []

discoverOpsCodingRec
  :: M.Map Int [InstructionTest]
  -> [(Int, (S.Set Op))]
  -> [(Int, Op)]
  -> Maybe (M.Map Int Op)
discoverOpsCodingRec operationsTests candidates mappings
  | candidates == [] =
      returnResult operationsTests mappings
  | otherwise =
      let ((opIdVal, opsSet):cs) = candidates
      in  join
            $ L.find (/= Nothing)
            $ L.map
                (\op ->
                  discoverOpsCodingRec
                    operationsTests
                    cs
                    ((opIdVal, op) : mappings))
            $ L.filter
                (\op ->
                  verifyOperationMapping
                    operationsTests
                    (opIdVal, op))
            $ filterOutMappingsOps opsSet mappings

filterOutMappingsOps
  :: S.Set Op
  -> [(Int, Op)]
  -> [Op]
filterOutMappingsOps opsSet =
  S.elems
  . S.difference opsSet
  . S.fromList
  . L.map snd

returnResult
  :: M.Map Int [InstructionTest]
  -> [(Int, Op)]
  -> Maybe (M.Map Int Op)
returnResult operationsTests mappings =
  let sizesMatch =
        M.size operationsTests == length mappings
      mappingCorrect =
        verifyOperationMapping operationsTests $ head mappings
  in
    if sizesMatch && mappingCorrect then
      Just $ M.fromList mappings
    else
      Nothing

verifyOperationMapping
  :: M.Map Int [InstructionTest]
  -> (Int, Op)
  -> Bool
verifyOperationMapping instructionTests (opIdVal, op) =
  L.and
  $ getTestResults
  $ L.map
      (flip executeInstructionTest $ op)
  $ instructionTests M.! opIdVal
  where
    getTestResults = L.map snd

generateOperationsCandidatesHistogram
  :: [InstructionTest]
  -> M.Map Int (S.Set Op)
generateOperationsCandidatesHistogram =
  L.foldr updateAmbiguousInstructionsDict M.empty

updateAmbiguousInstructionsDict
  :: InstructionTest
  -> M.Map Int (S.Set Op)
  -> M.Map Int (S.Set Op)
updateAmbiguousInstructionsDict instructionTest ambiguousDict =
  insertOperationsSet (opId instructionTest) ambiguousDict
  $ constructOperationsSet
  $ getSuccessfullTests
  $ L.map
      (executeInstructionTest instructionTest)
      operations

constructOperationsSet :: [Op] -> S.Set Op
constructOperationsSet = S.fromList

insertOperationsSet
  :: Int
  -> M.Map Int (S.Set Op)
  -> S.Set Op
  -> M.Map Int (S.Set Op)
insertOperationsSet operationId dict operations =
  M.insertWith S.union operationId operations dict

insertSample
  :: InstructionTest
  -> M.Map Int [InstructionTest]
  -> M.Map Int [InstructionTest]
insertSample test =
  M.insertWith (++) (opId test) [test]

executeOp :: Op -> OpInput -> Registers -> Registers
executeOp ADDR (a, b, c) regs = update regs c $ (+)   (regs !! a) (regs !! b)
executeOp ADDI (a, b, c) regs = update regs c $ (+)   (regs !! a) b
executeOp MULR (a, b, c) regs = update regs c $ (*)   (regs !! a) (regs !! b)
executeOp MULI (a, b, c) regs = update regs c $ (*)   (regs !! a) b
executeOp BANR (a, b, c) regs = update regs c $ (.&.) (regs !! a) (regs !! b)
executeOp BANI (a, b, c) regs = update regs c $ (.&.) (regs !! a) b
executeOp BORR (a, b, c) regs = update regs c $ (.|.) (regs !! a) (regs !! b)
executeOp BORI (a, b, c) regs = update regs c $ (.|.) (regs !! a) b
executeOp SETR (a, _, c) regs = update regs c $       (regs !! a)
executeOp SETI (a, _, c) regs = update regs c $       a
executeOp GTIR (a, b, c) regs = update regs c $ b2i $ (>)  a                (regs !! b)
executeOp GTRI (a, b, c) regs = update regs c $ b2i $ (>)  (regs !! a) b
executeOp GTRR (a, b, c) regs = update regs c $ b2i $ (>)  (regs !! a) (regs !! b)
executeOp EQIR (a, b, c) regs = update regs c $ b2i $ (==) a                (regs !! b)
executeOp EQRI (a, b, c) regs = update regs c $ b2i $ (==) (regs !! a) b
executeOp EQRR (a, b, c) regs = update regs c $ b2i $ (==) (regs !! a) (regs !! b)

update :: Registers -> Int -> Int -> Registers
update regs reg val =
  L.take reg regs ++ [ val ] ++ L.drop (reg + 1) regs

b2i :: Bool -> Int
b2i False = 0
b2i True  = 1

countCandidates :: InstructionTest -> Int
countCandidates test =
  L.length $
  getSuccessfullTests $
  L.map (executeInstructionTest test) operations

findAmbiguousSamples :: [InstructionTest] -> [(InstructionTest, Int)]
findAmbiguousSamples =
  L.filter ((>= 3) . snd) .
  L.map (\t -> (t, countCandidates t))

getSuccessfullTests :: [(Op, Bool)] -> [Op]
getSuccessfullTests = map fst . filter snd

executeInstructionTest :: InstructionTest -> Op -> (Op, Bool)
executeInstructionTest test op =
  (op, result)
  where
    result = lhs == rhs
    lhs = executeOp op (input test) registersIn
    registersIn = before test
    rhs = after test

parseInstructionsTests :: String -> [InstructionTest]
parseInstructionsTests =
  L.map parseInstructionTest
  . groupBy 3 []
  . L.filter ((/=) "")
  . lines

parseInstructionsTestsB
  :: [[String]]
  -> [InstructionTest]
parseInstructionsTestsB =
  L.map parseInstructionTest

parseInstructionTest :: [String] -> InstructionTest
parseInstructionTest [bl, ol, al] =
  InstructionTest
    { opId = parseOpId ol
    , input = parseInput ol
    , before = parseRegisters bl
    , after = parseRegisters al
    }

parseInstruction
  :: String
  -> (Int, OpInput)
parseInstruction str =
  let
    nums    = words str
    opIdVal = (read :: String -> Int) $ L.head nums
    input   = parseInput $ unwords nums
  in
    (opIdVal, input)

parseOpId :: String -> Int
parseOpId =
  (read :: String -> Int)
  . L.head
  . words

parseInput :: String -> OpInput
parseInput str =
  let
    [a, b, c] =
      L.map (read :: String -> Int) $
      L.tail $
      words str
  in
    (a, b, c)

parseRegisters :: String -> Registers
parseRegisters =
    (read :: String -> [Int]) .
    unwords .
    L.tail .
    words

groupBy
  :: Eq a
  => Int
  -> [[a]]
  -> [a]
  -> [[a]]
groupBy n groups lines'
  | lines' == [] =
      reverse groups
  | otherwise    =
      groupBy n (L.take n lines' : groups) (L.drop n lines')

sample = "Before: [3, 2, 1, 1]\n9 2 1 2\nAfter:  [3, 2, 2, 1]"

sampleBig = "Before: [3, 1, 2, 0]\n5 1 2 0\nAfter:  [0, 1, 2, 0]\n\nBefore: [3, 3, 0, 2]\n10 2 0 1\nAfter:  [3, 0, 0, 2]"
