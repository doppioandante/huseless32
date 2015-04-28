module Huseless.InstructionSet.Arithmetic where

import Control.Monad.State

import Huseless.Common
import Huseless.Instruction
import Huseless.InstructionSet.Validation
import Huseless.System
import Huseless.System.PD32
import Huseless.System.StatusRegister

arithmeticInstructionTable :: Monad m => [(Int, Instruction -> System m())]
arithmeticInstructionTable = [
    (0, opADD)
    ]

arithMatcher = defaultMatcher `matchSize` mAll `matchSM` mAll `matchDM` mRegister

opADD :: Monad m => Instruction -> System m ()
opADD instr = do
    (_, _, size, sourceAMode, reg) <- match instr arithMatcher
    s <- readSource sourceAMode size
    r <- readRegister reg
    res <- aluCompute (+) (extendSign size s) (extendSign size r)
    writeRegister reg (mask size res)

--opADD :: Monad m => Instruction -> System m ()
--opADD instr = do
--    (_, _, size, sourceAMode, reg) <- match instr arithMatcher
--    s <- readSource sourceAMode size
--    r <- readRegister reg
--    res <- aluCompute (+) (extendSign size s) (extendSign size r)
--    writeRegister reg (mask size res)
