module Day21.Day21 where

import Data.Bits ((.&.), (.|.))
import Data.Char (digitToInt)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Vector (Vector)
import qualified Data.Vector as V

type Register = Int -- from 0 to 5
type Registers = Vector Int -- 6 arbitrary numbers
type OpInput = (Int, Int, Int)
data OpType
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

type Op = (OpType, OpInput)
type Program = (Register, Vector Op)


solveA
  :: String
  -> Int
solveA input =
  let program =
        parseProgram input
      haltingInstruction =
        findHaltingInstruction
      registerToTrack =
        findHaltingRegister
      resultRegisters =
        executeUntilFirstOccuranceOfInstruction
          program
          haltingInstruction
  in  resultRegisters V.! registerToTrack

solveB
  :: String
  -> Int
solveB input =
  let program =
        parseProgram input
      haltingInstruction =
        findHaltingInstruction
      registerToTrack =
        findHaltingRegister
  in getLastValueBeforeFirstDuplicate
       program
       haltingInstruction
       registerToTrack

executeUntilFirstOccuranceOfInstruction
  :: Program
  -> Int
  -> Registers
executeUntilFirstOccuranceOfInstruction
  program@(ipReg, _)
  haltingInstruction =
    executeUntil
      program
      ((== haltingInstruction) . (V.! ipReg))

-- Get last value of register to check before first duplicated value occurs
getLastValueBeforeFirstDuplicate
  :: Program
  -> Int
  -> Register
  -> Int
getLastValueBeforeFirstDuplicate
  program@(ipReg, _)
  haltingInstruction
  registerToTrack =
  go initLastTrackedRegVal initRegValuesSoFar intitRegisters
  where
    initLastTrackedRegVal =
      -1
    initRegValuesSoFar =
      S.empty
    go
      :: Int
      -> Set Int
      -> Registers
      -> Int
    go lastTrackedRegVal regValuesSoFar registers
      | isEvaluatingHaltingInstruction && isTrackedValDuplicated =
          lastTrackedRegVal
      | isEvaluatingHaltingInstruction && not isTrackedValDuplicated =
          go
            trackedRegVal
            (S.insert trackedRegVal regValuesSoFar)
            (executeInstruction program registers)
      | otherwise =
          go
            lastTrackedRegVal
            regValuesSoFar
            (executeInstruction program registers)
      where
        isEvaluatingHaltingInstruction =
          registers V.! ipReg == haltingInstruction
        isTrackedValDuplicated =
          S.member trackedRegVal regValuesSoFar
        trackedRegVal =
          registers V.! registerToTrack

executeUntil
  :: Program
  -> (Registers -> Bool)
  -> Registers
executeUntil
  program
  haltingPred =
  go intitRegisters
  where
    go :: Registers -> Registers
    go registers
      | haltingPred registers =
          registers
      | otherwise =
          go $ executeInstruction program registers

findHaltingInstruction
  :: Int
findHaltingInstruction =
  -- Find any instruction modifying IPReg, which might cause to point it
  -- beyond given instruction set;
  -- Assumption - there is no jump to that instruction from place other than
  -- previous line (checked manually)
  -- addr 2 1 1
  29

findHaltingRegister
  :: Register
findHaltingRegister =
  -- Register used by instruction, which sets the Register involved in
  -- stopping the program (0, continue, 1 stop).
  -- If R0 is equal to value in register numbered:
  -- eqrr 5 0 2
  5

intitRegisters
  :: Registers
intitRegisters =
  V.replicate 6 0

-- Executes instruction and updates the registers accordingly
executeInstruction
  :: Program
  -> Registers
  -> Registers
executeInstruction (ipReg, instructions) registers =
  if ipVal >= V.length instructions then
    registers
  else
    let preOpRegisters =
          update registers ipReg ipVal
        operation =
          instructions V.! ipVal
        postOpRegisters =
          executeOp operation preOpRegisters
        newIpVal =
          (postOpRegisters V.! ipReg) + 1
    in  update postOpRegisters ipReg newIpVal
  where
    ipVal =
      registers V.! ipReg

-- Only executes instruction
executeOp
  :: Op
  -> Registers
  -> Registers
executeOp (ADDR, (a, b, c)) regs =
  update regs c $ (+)   (regs V.! a) (regs V.! b)
executeOp (ADDI, (a, b, c)) regs =
  update regs c $ (+)   (regs V.! a) b
executeOp (MULR, (a, b, c)) regs =
  update regs c $ (*)   (regs V.! a) (regs V.! b)
executeOp (MULI, (a, b, c)) regs =
  update regs c $ (*)   (regs V.! a) b
executeOp (BANR, (a, b, c)) regs =
  update regs c $ (.&.) (regs V.! a) (regs V.! b)
executeOp (BANI, (a, b, c)) regs =
  update regs c $ (.&.) (regs V.! a) b
executeOp (BORR, (a, b, c)) regs =
  update regs c $ (.|.) (regs V.! a) (regs V.! b)
executeOp (BORI, (a, b, c)) regs =
  update regs c $ (.|.) (regs V.! a) b
executeOp (SETR, (a, _, c)) regs =
  update regs c $       (regs V.! a)
executeOp (SETI, (a, _, c)) regs =
  update regs c $       a
executeOp (GTIR, (a, b, c)) regs =
  update regs c $ b2i $ (>)  a            (regs V.! b)
executeOp (GTRI, (a, b, c)) regs =
  update regs c $ b2i $ (>)  (regs V.! a) b
executeOp (GTRR, (a, b, c)) regs =
  update regs c $ b2i $ (>)  (regs V.! a) (regs V.! b)
executeOp (EQIR, (a, b, c)) regs =
  update regs c $ b2i $ (==) a            (regs V.! b)
executeOp (EQRI, (a, b, c)) regs =
  update regs c $ b2i $ (==) (regs V.! a) b
executeOp (EQRR, (a, b, c)) regs =
  update regs c $ b2i $ (==) (regs V.! a) (regs V.! b)

update
  :: Registers
  -> Int
  -> Int
  -> Registers
update regs reg val =
  regs V.// [(reg, val)]

b2i
  :: Bool
  -> Int
b2i False = 0
b2i True  = 1

parseProgram
  :: String
  -> Program
parseProgram input =
    let lns = lines input
    in ( parseIp (head lns)
       , V.fromList $
           map parseOp (tail lns)
       )
    where
      parseIp line =
        digitToInt $ (last line) :: Register

parseOp
  :: String
  -> Op
parseOp line =
  ( parseOpType (head chunks)
  , parseInput (tail chunks)
  )
  where
    chunks = words line

parseOpType
  :: String
  -> OpType
parseOpType "addr" = ADDR
parseOpType "addi" = ADDI
parseOpType "mulr" = MULR
parseOpType "muli" = MULI
parseOpType "banr" = BANR
parseOpType "bani" = BANI
parseOpType "borr" = BORR
parseOpType "bori" = BORI
parseOpType "setr" = SETR
parseOpType "seti" = SETI
parseOpType "gtir" = GTIR
parseOpType "gtri" = GTRI
parseOpType "gtrr" = GTRR
parseOpType "eqir" = EQIR
parseOpType "eqri" = EQRI
parseOpType "eqrr" = EQRR

parseInput
  :: [String]
  -> OpInput
parseInput strs =
  let
    [a, b, c] =
      map (read :: String -> Int) strs
  in
    (a, b, c)

input :: String
input =
  "#ip 1\nseti 123 0 5\nbani 5 456 5\neqri 5 72 5\naddr 5 1 1\nseti 0 0 1\nseti 0 2 5\nbori 5 65536 4\nseti 3935295 1 5\nbani 4 255 2\naddr 5 2 5\nbani 5 16777215 5\nmuli 5 65899 5\nbani 5 16777215 5\ngtir 256 4 2\naddr 2 1 1\naddi 1 1 1\nseti 27 1 1\nseti 0 5 2\naddi 2 1 3\nmuli 3 256 3\ngtrr 3 4 3\naddr 3 1 1\naddi 1 1 1\nseti 25 0 1\naddi 2 1 2\nseti 17 7 1\nsetr 2 2 4\nseti 7 6 1\neqrr 5 0 2\naddr 2 1 1\nseti 5 4 1"
