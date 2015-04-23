module InstructionSet.Arithmetic where

import Common
import Instruction
import System

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
