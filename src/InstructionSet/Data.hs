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
opMOV instr =
    match instr (defaultMatcher `matchSize` mAll `matchSM` mAll `matchDM` mAll) >>
    readSource (sourceAMode instr) (size instr) >>=
    doSignExtension instr >>=
    writeDest (destAMode instr) (size instr)

opMOVFRSR :: Monad m => Instruction -> System m ()
opMOVFRSR instr =
    match instr (defaultMatcher `matchDM` mAll) >>
    readSR >>= writeDest (destAMode instr) ZLWord

opMOVTOSR :: Monad m => Instruction -> System m ()
opMOVTOSR instr =
    match instr (defaultMatcher `matchSM` mAll) >>
    readSource (sourceAMode instr) ZLWord >>= writeSR

opMVL :: Monad m => Instruction -> System m ()
opMVL instr =
    match instr (defaultMatcher `matchSize` mAll `matchSM` mAll `matchDM` mAll) >>
    readSource (sourceAMode instr) (size instr) >>=
    writeDest (destAMode instr) (size instr)

opEXG :: Monad m => Instruction -> System m ()
opEXG instr = do
    -- NOTE: SM decoding gets done by writeDest below
    (_, _, size, _, reg) <- match instr (defaultMatcher `matchSize` mAll
                                         `matchDM` mRegister)
    mem <- readSource (sourceAMode instr) size
    regValue <- readRegister reg
    writeDest (sourceAMode instr) size regValue
    writeRegister reg $ extendSign size mem


