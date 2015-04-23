module InstructionSet.Control
where

import Control.Monad.Except

import Common
import Instruction
import System

controlInstructionTable :: Monad m => [(Int, Instruction -> System m ())]
controlInstructionTable = [
    (0, opHALT),
    (1, opNOP),
    (2, opRESET),
    (3, opTRAP),
    (31, opTRACE)
    ]

opHALT :: Monad m => Instruction -> System m ()
opHALT _ = throwError HaltExecution

opNOP :: Monad m => Instruction -> System m ()
opNOP _ = return ()

opRESET :: Monad m => Instruction -> System m ()
opRESET _ = undefined

opTRAP :: Monad m => Instruction -> System m ()
opTRAP _ = undefined

opTRACE :: Monad m => Instruction -> System m ()
opTRACE _ = throwError Trace
