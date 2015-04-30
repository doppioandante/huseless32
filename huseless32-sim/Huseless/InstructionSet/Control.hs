module Huseless.InstructionSet.Control
where

import Control.Monad.Except

import Huseless.Common
import Huseless.Instruction
import Huseless.InstructionSet.Validation
import Huseless.System

controlInstructionTable :: Monad m => [(Int, Instruction -> System m ())]
controlInstructionTable = [
    (0, opHALT),
    (1, opNOP),
    (2, opRESET),
    (3, opTRAP),
    (31, opTRACE)
    ]

opHALT :: Monad m => Instruction -> System m ()
opHALT instr = match instr defaultMatcher >> throwError HaltExecution

opNOP :: Monad m => Instruction -> System m ()
opNOP instr = match instr defaultMatcher >> return ()

-- What the hell is this supposed to do
opRESET :: Monad m => Instruction -> System m ()
opRESET _ = undefined

-- REMEMBER: needs parameter
opTRAP :: Monad m => Instruction -> System m ()
opTRAP _ = undefined

opTRACE :: Monad m => Instruction -> System m ()
opTRACE instr = match instr defaultMatcher >> throwError Trace
