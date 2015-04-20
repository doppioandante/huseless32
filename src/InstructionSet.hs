module InstructionSet
where

import Common
import Instruction
import InstructionSet.Control
import InstructionSet.Data
import SystemState

getAction :: Int -> Word8 -> Maybe (Instruction -> SystemState)
getAction instrClass instrCode = do
    instrTable <-  if instrClass <= 7
                      then Just $ instructionTables !! instrClass
                      else Nothing
    lookup instrCode instrTable

instructionTables :: [[(Word8, (Instruction -> SystemState))]]
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

