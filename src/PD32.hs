module PD32 where


import Common
import Device
import Memory.VectorMemory

type SysMemory = VectorMemory
type LegalAddress = Int

data StatusRegister = StatusRegister
    {
        carry :: Bool,
        negative :: Bool,
        zero :: Bool,
        overflow :: Bool,
        parity :: Bool,
        interrupt :: Bool
    }
    deriving (Eq, Show, Read)


data PD32 = PD32
    {
        pc   :: LWord, -- program counter

        cpuRegisters :: [LWord],
        statusRegister :: StatusRegister,
        memory :: SysMemory,
        devices :: [Device]
    }
    deriving (Eq, Show)




