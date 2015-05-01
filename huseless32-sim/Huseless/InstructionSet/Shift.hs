module Huseless.InstructionSet.Shift
where

import Control.Monad.State

import Huseless.Instruction
import Huseless.System

shiftInstructionTable :: Monad m => [(Int, Instruction -> System m ())]
shiftInstructionTable = [
    (0, opASL),
    (1, opASR),
    (2, opLSL),
    (3, opLSR),
    (4, opRCL),
    (5, opRCR),
    (6, opROL),
    (7, opROR)
    ]

opASL :: Monad m => Instruction -> System m ()
opASL = undefined

opASR :: Monad m => Instruction -> System m ()
opASR = undefined
opLSL :: Monad m => Instruction -> System m ()
opLSL = undefined
opLSR :: Monad m => Instruction -> System m ()
opLSR = undefined
opRCL :: Monad m => Instruction -> System m ()
opRCL = undefined
opRCR :: Monad m => Instruction -> System m ()
opRCR = undefined
opROL :: Monad m => Instruction -> System m ()
opROL = undefined
opROR :: Monad m => Instruction -> System m ()
opROR = undefined
