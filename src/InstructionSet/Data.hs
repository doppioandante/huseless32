module InstructionSet.Data
where

import Control.Monad.Except

import Common
import Instruction
import InstructionSet.Control
import InstructionSet.Validation
import System

dataInstructionTable :: Monad m => [(Int, Instruction -> System m ())]
dataInstructionTable = [
    (0, opMOV),
    (1, opMOVFRSR),
    (2, opMOVTOSR),
    (3, opMVL),
    (4, opEXG)
    ]

-- TODO: set status resgister
opMOV :: Monad m => Instruction -> System m ()
opMOV instr = readSource (sourceAMode instr) (size instr)
              >>= doSignExtension instr
              >>= writeDest (destAMode instr) (size instr)

opMOVFRSR :: Monad m => Instruction -> System m ()
opMOVFRSR instr = readSR >>= writeDest (destAMode instr) ZLWord

opMOVTOSR :: Monad m => Instruction -> System m ()
opMOVTOSR instr = readSource (sourceAMode instr) ZLWord >>= writeSR

opMVL :: Monad m => Instruction -> System m ()
opMVL instr = readSource (sourceAMode instr) (size instr)
              >>= writeDest (destAMode instr) (size instr)

-- FIXME: must not exchange with dst = memory location
opEXG :: Monad m => Instruction -> System m ()
opEXG instr = do
    mem <- readSource (sourceAMode instr) (size instr)
    reg <- readSource (destAMode instr) (size instr)
    writeDest (sourceAMode instr) (size instr) reg
    writeDest (destAMode instr) (size instr) (extendSign (size instr) mem)


