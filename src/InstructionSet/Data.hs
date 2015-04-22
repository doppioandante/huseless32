module InstructionSet.Data
where

import Common
import Instruction
import InstructionSet.Control
import System

dataInstructionTable :: Monad m => [(Word8, Instruction -> System m ())]
dataInstructionTable = [
    (0, opMOV),
    (1, opNOP),
    (2, opNOP),
    (3, opNOP),
    (4, opNOP)
    ]

-- TODO: set status resgister
opMOV :: Monad m => Instruction -> System m ()
opMOV instr = readSource (sourceAMode instr) (size instr)
              >>= doSignExtension instr
              >>= writeDest (destAMode instr) (size instr)
