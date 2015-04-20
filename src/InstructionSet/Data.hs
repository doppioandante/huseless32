module InstructionSet.Data
where

import Common
import Instruction
import InstructionSet.Control
import SystemState

dataInstructionTable :: [(Word8, Instruction -> SystemState)]
dataInstructionTable = [
    (0, opNOP),
    (1, opNOP),
    (2, opNOP),
    (3, opNOP),
    (4, opNOP)
    ]

--opMOV instr = do
    --state <- get
    --put state
