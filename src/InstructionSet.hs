module InstructionSet
where

import Common
import Instruction
import InstructionSet.Control
import InstructionSet.Data
import System

getAction :: Monad m => Int -> Word8 -> Maybe (Instruction -> System m ())
getAction instrClass instrCode = do
    instrTable <-  if instrClass <= 7
                      then Just $ instructionTables !! instrClass
                      else Nothing
    lookup instrCode instrTable

instructionTables :: Monad m => [[(Word8, (Instruction -> System m ()))]]
instructionTables = [
    controlInstructionTable,
    dataInstructionTable
    ]
--    arithmeticInstructionTable,
--    logicInstructionTable,
--    shiftInstructionTable,
--    condBitsInstructionTable,
--    execFlowInstructionTable,
--    inOutInstructionTable

