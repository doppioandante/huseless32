module InstructionSet.Arithmetic where

import Control.Monad.State

import Common
import Instruction
import System
import System.PD32
import System.StatusRegister

arithmeticInstructionTable :: Monad m => [(Int, Instruction -> System m())]
arithmeticInstructionTable = [
    (0, opADD)
    ]

opADD :: Monad m => Instruction -> System m ()
opADD instr = do
    s <- readSource (sourceAMode instr) (size instr)
    r <- readSource (destAMode instr) (ZLWord)
    res <- aluCompute (+) s r
    writeDest (destAMode instr) (ZLWord) res

opADC :: Monad m => Instruction -> System m ()
opADC instr = undefined --do
--    PD32 { statusRegister = (StatusRegister c _ _ _ _ _) } <- get
    
