module Huseless.InstructionSet.CondBits
where

import Huseless.Instruction
import Huseless.InstructionSet.Validation
import Huseless.System

condBitsInstructionTable :: Monad m => [(Int, Instruction -> System m ())]
condBitsInstructionTable = [
    (0, opCLRC),
    (1, opCLRN),
    (2, opCLRZ),
    (3, opCLRV),
    (4, opCLRP),
    (5, opCLRI),
    (16, opSETC),
    (17, opSETN),
    (18, opSETZ),
    (19, opSETV),
    (20, opSETP),
    (21, opSETI)
    ]

-- all fields must be filled with null bits
condBitsMatcher = defaultMatcher

opCLRC instr =
    match instr condBitsMatcher >>
    setCarry False

opCLRN instr =
    match instr condBitsMatcher >>
    setNegative False

opCLRZ instr =
    match instr condBitsMatcher >>
    setZero False

opCLRV instr =
    match instr condBitsMatcher >>
    setOverflow False

opCLRP instr =
    match instr condBitsMatcher >>
    setParity False

opCLRI instr =
    match instr condBitsMatcher >>
    setInterrupt False

opSETC instr =
    match instr condBitsMatcher >>
    setCarry False

opSETN instr =
    match instr condBitsMatcher >>
    setNegative True

opSETZ instr =
    match instr condBitsMatcher >>
    setZero True

opSETV instr =
    match instr condBitsMatcher >>
    setOverflow True

opSETP instr =
    match instr condBitsMatcher >>
    setParity True

opSETI instr =
    match instr condBitsMatcher >>
    setInterrupt True
