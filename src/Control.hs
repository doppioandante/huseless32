module ControlUnit
where

import Common
import CPU
import ExecutionContext
import Memory

-- in seconds
clock :: Double
clock = 1.0

writeRegister :: Int -> Int -> LWord -> ExecutionContext
writeRegister z reg value = do
    (cpu, m, dev) <- get
    let registers' = modifyRegister z (registers cpu) reg value
    let cpu' = CPU registers' (pc cpu) (statusRegister cpu)
    put (cpu', m, dev)

-- ?? needed ??
--readMemory :: RandomAccessible a b => Int -> a -> Address -> Maybe LWord
--readMemory z memory address = do
--    actualAddr <- checkAddress address memory
--    readZ z memory actualAddr

writeMemory :: Int -> LegalAddress -> LWord -> ExecutionContext
writeMemory z address value = do
    (cpu, mem, dev) <- get
    put (cpu, writeZ z mem address value, dev)

