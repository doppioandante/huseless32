module InstructionSet.Data
where

import Common
import Instruction
import InstructionSet.Control
import System

dataInstructionTable :: Monad m => [(Word8, Instruction -> System m ())]
dataInstructionTable = []
dataInstructionTable = [
    (0, opMOV),
    (1, opNOP),
    (2, opNOP),
    (3, opNOP),
    (4, opNOP)
    ]

opMOV :: Instruction -> System m ()
opMOV instr = do
    let sourceReg = fromIntegral (sourceRegister instr) :: Int
        destReg = fromIntegral (destRegister instr) :: Int
    case 
    readSource
