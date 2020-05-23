module Day19.Day19 where

import Data.Bits
import Data.Char (digitToInt)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.List as L

type Register = Int -- from 0 to 5
type Registers = Vector Int --6 arbitrary numbers
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


resultRegister :: Register
resultRegister = 0

solve
  :: Registers
  -> String
  -> Int -- Value of register resultRegister
solve registers =
  getRegisterVal resultRegister .
  executeProgram registers .
  parseProgram

solveB
  :: Registers
  -> String
  -> Int -- Value of register resultRegister
solveB registers input =
  calculateFinalValue
  $ executeUntil
      (hitBeginningOfStageTwo ipReg)
      registers
      program
  where
    program@(ipReg, _) =
      parseProgram input

calculateFinalValue
  :: Registers
  -> Int
calculateFinalValue registers =
  -- Stage two iterates throuph nested loops R3 inner R1 outer
  -- They span integers between 1 and value of R4 (which is constant)
  -- R0 is incremented by the value of R1 every time R1 * R3 == R4
  -- It means R0 accumulates all numbers which divide R4 mod 0
  L.sum
  $ L.filter
      (\x -> reg4 `mod` x == 0)
      valuesRange
  where
    valuesRange =
      [1..reg4]
    reg4 =
      getRegisterVal
        4
        registers

hitBeginningOfStageTwo
  :: Register
  -> Registers
  -> Bool
hitBeginningOfStageTwo ipReg registers =
  -- Input program is divided into two stages
  -- Stage 1 is preparing registers (instructions 0, 17-(26|35)
  -- 26 for part A
  -- 35 for part B
  -- Second stage starts in instruction 1
  getRegisterVal ipReg registers
    == instructionStartingStageTwo
  where
    instructionStartingStageTwo = 1

executeUntil
  :: (Registers -> Bool)
  -> Registers
  -> Program
  -> Registers
executeUntil
  haltingPred
  initialRegs
  program =
  go initialRegs
  where
    go :: Registers -> Registers
    go registers
      | haltingPred registers =
          registers
      | otherwise =
          go $ executeInstruction program registers

executeOp
  :: Op
  -> Registers
  -> Registers
executeOp (ADDR, (a, b, c)) regs = update regs c $ (+)   (regs V.! a) (regs V.! b)
executeOp (ADDI, (a, b, c)) regs = update regs c $ (+)   (regs V.! a) b
executeOp (MULR, (a, b, c)) regs = update regs c $ (*)   (regs V.! a) (regs V.! b)
executeOp (MULI, (a, b, c)) regs = update regs c $ (*)   (regs V.! a) b
executeOp (BANR, (a, b, c)) regs = update regs c $ (.&.) (regs V.! a) (regs V.! b)
executeOp (BANI, (a, b, c)) regs = update regs c $ (.&.) (regs V.! a) b
executeOp (BORR, (a, b, c)) regs = update regs c $ (.|.) (regs V.! a) (regs V.! b)
executeOp (BORI, (a, b, c)) regs = update regs c $ (.|.) (regs V.! a) b
executeOp (SETR, (a, _, c)) regs = update regs c $       (regs V.! a)
executeOp (SETI, (a, _, c)) regs = update regs c $       a
executeOp (GTIR, (a, b, c)) regs = update regs c $ b2i $ (>)  a            (regs V.! b)
executeOp (GTRI, (a, b, c)) regs = update regs c $ b2i $ (>)  (regs V.! a) b
executeOp (GTRR, (a, b, c)) regs = update regs c $ b2i $ (>)  (regs V.! a) (regs V.! b)
executeOp (EQIR, (a, b, c)) regs = update regs c $ b2i $ (==) a            (regs V.! b)
executeOp (EQRI, (a, b, c)) regs = update regs c $ b2i $ (==) (regs V.! a) b
executeOp (EQRR, (a, b, c)) regs = update regs c $ b2i $ (==) (regs V.! a) (regs V.! b)

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

getRegisterVal
  :: Register
  -> Registers
  -> Int
getRegisterVal =
  flip (V.!)

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

executeProgram
  :: Registers
  -> Program
  -> Registers
executeProgram registers (ipReg, instructions) =
  executeProgram'
    instructions
    registers
    (registers V.! ipReg)
  where
    instructionsLen =
      V.length instructions
    executeProgram'
      instructions
      registers
      ipVal =
        if ipVal >= instructionsLen
        then registers
        else
          let preOpRegisters =
                update registers ipReg ipVal
              postOpRegisters =
                executeOp (instructions V.! ipVal) preOpRegisters
              newIpVal =
                (postOpRegisters V.! ipReg) + 1
          in  executeProgram'
                instructions
                postOpRegisters
                newIpVal

sample19 :: String
sample19 = "#ip 0\nseti 5 0 1\nseti 6 0 2\naddi 0 1 0\naddr 1 2 3\nsetr 1 0 0\nseti 8 0 4\nseti 9 0 5"
