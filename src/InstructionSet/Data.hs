module InstructionSet.Data
where

import Control.Monad.Except

import Common
import Instruction
import InstructionSet.Control
import System

dataInstructionTable :: Monad m => [(Word8, Instruction -> System m ())]
dataInstructionTable = [
    (0, opMOV),
    (1, opNOP),
    (2, opNOP),
    (3, opMVL),
    (4, opEXG)
    ]

-- TODO: set status resgister
opMOV :: Monad m => Instruction -> System m ()
opMOV instr = readSource (sourceAMode instr) (size instr)
              >>= doSignExtension instr
              >>= writeDest (destAMode instr) (size instr)

opMVL :: Monad m => Instruction -> System m ()
opMVL instr = readSource (sourceAMode instr) (size instr)
              >>= writeDest (destAMode instr) (size instr)

opEXG :: Monad m => Instruction -> System m ()
opEXG instr = do
    mem <- readSource (sourceAMode instr) (size instr)
    reg <- readSource (destAMode instr) (size instr)
    writeDest (sourceAMode instr) (size instr) reg
    writeDest (destAMode instr) (size instr) (extendSign (size instr) mem)


