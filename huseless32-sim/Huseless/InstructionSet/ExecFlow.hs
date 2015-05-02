module Huseless.InstructionSet.ExecFlow
where

import Control.Monad.State

import Huseless.Instruction
import Huseless.InstructionSet.Validation
import Huseless.System
import Huseless.System.StatusRegister
import Huseless.System.PD32

execFlowInstructionTable :: Monad m => [(Int, Instruction -> System m ())]
execFlowInstructionTable = [
    (0, opJMP),
    (1, opJSR),
    (2, opRET),
    (3, opRTI),
    (4, opConditionalJump carry True), -- JC
    (5, opConditionalJump negative True), -- JN
    (6, opConditionalJump zero True), -- JZ
    (7, opConditionalJump overflow True), -- JV
    (8, opConditionalJump parity True), -- JP
    (9, opConditionalJump interrupt True), -- JI
    (10, opConditionalJump carry False), -- JCC
    (11, opConditionalJump negative False), -- JNN
    (12, opConditionalJump zero False), -- JNZ
    (13, opConditionalJump overflow False), -- JNV
    (14, opConditionalJump parity False), -- JNP
    (15, opConditionalJump interrupt False)  -- JNI
    ]

jumpMatcher = defaultMatcher `matchSize` mExact ZLWord `matchDM` mAll

opJMP :: Monad m => Instruction -> System m ()
opJMP instr = do
    match instr jumpMatcher

    location <- readSource (destAMode instr) ZLWord
    writePC location

opJSR :: Monad m => Instruction -> System m ()
opJSR instr = do
    -- PUSH PC = MOVL PC, -(R7)
    pd32 <- get
    stackPointer <- readRegister 7
    let newSP = stackPointer - 4 -- decrement stackPointer
    writeRegister 7 newSP
    writeMemory ZLWord newSP $ pc pd32

    opJMP instr

opRET :: Monad m => Instruction -> System m ()
opRET instr = do
    -- POP PC = MOVL (R7)+, PC
    pd32 <- get
    stackPointer <- readRegister 7
    newPc <- readMemory ZLWord stackPointer
    writeRegister 7 (stackPointer+4)

    writePC newPc

opRTI :: Monad m => Instruction -> System m ()
opRTI instr = do
    -- POP PC
    pd32 <- get
    stackPointer <- readRegister 7
    newPc <- readMemory ZLWord stackPointer

    writePC newPc

    -- POP SR
    readMemory ZLWord (stackPointer+4) >>= writeSR
    writeRegister 7 (stackPointer+8)

opConditionalJump :: Monad m => (StatusRegister -> Bool) -> Bool ->
                     Instruction -> System m ()
opConditionalJump test expected instr = do
    pd32 <- get
    let sr = statusRegister pd32
    if test sr == expected
       then opJMP instr
       else return ()
