module Huseless.System.PD32 where


import Huseless.Common
import Huseless.System.Device
import Huseless.System.Memory.VectorMemory
import Huseless.System.StatusRegister

type SysMemory = VectorMemory
type LegalAddress = Int

data PD32 = PD32
    {
        pc   :: LWord, -- program counter

        cpuRegisters :: [LWord],
        statusRegister :: StatusRegister,
        memory :: SysMemory,
        devices :: [Device]
    }
    deriving (Eq, Show)




