module InstructionSet.Control
where

import Control.Monad.Except

import Common
import Instruction
import System

controlInstructionTable :: Monad m => [(Word8, Instruction -> System m ())]
controlInstructionTable = [
    (0, opNOP),
    (1, opNOP),
    (2, opNOP),
    (3, opNOP),
    (31, opNOP)
    ]

opHALT :: Monad m => Instruction -> System m ()
opHalt _ = throwError HaltExecution

opNOP :: Monad m => Instruction -> System m ()
opNOP _ = return ()

opRESET :: Monad m => Instruction -> System m ()
opRESET _ = undefined

opTRAP :: Monad m => Instruction -> System m ()
opTRAP = undefined

opTRACE :: Monad m => Instruction -> System m ()
opTRACE = throwError Trace
