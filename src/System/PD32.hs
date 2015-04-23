module System.PD32 where


import Common
import System.Device
import System.Memory.VectorMemory
import System.StatusRegister

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




