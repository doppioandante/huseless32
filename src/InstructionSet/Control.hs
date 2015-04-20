module InstructionSet.Control
where


import Common
import Instruction
import SystemState

controlInstructionTable :: [(Word8, Instruction -> SystemState)]
controlInstructionTable = [
    (0, opNOP),
    (1, opNOP),
    (2, opNOP),
    (3, opNOP),
    (31, opNOP)
    ]

opNOP :: Instruction -> SystemState
opNOP _ = do return ()


