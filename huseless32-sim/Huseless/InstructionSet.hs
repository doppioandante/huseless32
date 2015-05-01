module Huseless.InstructionSet
where

import Huseless.Common
import Huseless.Instruction

import Huseless.InstructionSet.ArithLogic
import Huseless.InstructionSet.Control
import Huseless.InstructionSet.Data

import Huseless.System

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
    arithmeticInstructionTable,
    undefined,
    undefined,
    condBitsInstructionTable
    ]
--    logicInstructionTable,
--    shiftInstructionTable,
--    condBitsInstructionTable,
--    execFlowInstructionTable,
--    inOutInstructionTable

