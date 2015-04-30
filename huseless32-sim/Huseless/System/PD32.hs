module Huseless.System.PD32 where

import Huseless.Common
import Huseless.System.Device
import Huseless.System.Memory
import Huseless.System.StatusRegister

data PD32 = PD32
    {
        pc   :: LWord, -- program counter

        cpuRegisters :: [LWord],
        statusRegister :: StatusRegister,
        memory :: AnyMem,
        devices :: [Device]
    }




