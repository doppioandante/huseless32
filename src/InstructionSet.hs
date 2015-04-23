module InstructionSet
where

import Common
import Instruction

import InstructionSet.Arithmetic
import InstructionSet.Control
import InstructionSet.Data

import System

getAction :: Monad m => Int -> Int -> Maybe (Instruction -> System m ())
getAction instrClass instrCode = do
    instrTable <-  if instrClass <= 7
                      then Just $ instructionTables !! instrClass
                      else Nothing
    lookup instrCode instrTable

instructionTables :: Monad m => [[(Int, (Instruction -> System m ()))]]
instructionTables = [
    controlInstructionTable,
    dataInstructionTable,
    arithmeticInstructionTable
    ]
--    logicInstructionTable,
--    shiftInstructionTable,
--    condBitsInstructionTable,
--    execFlowInstructionTable,
--    inOutInstructionTable

