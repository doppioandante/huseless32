module SystemState
where

import Control.Monad.State.Lazy

import Common
import ControlUnit
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
        ctrlUnit :: ControlUnit,
        cpuRegisters :: [Register],
        statusRegister :: StatusRegister,
        memory :: SysMemory,
        devices :: [Device]
    }
    deriving (Eq, Show)

type SystemState = StateT PD32 IO ()

getMask :: PD32 -> (LWord -> LWord)
getMask = maskZ . size . ctrlUnit

-- TODO: check register index
readRegister :: PD32 -> Int -> LWord
readRegister sys idx = (cpuRegisters sys) !! idx

modifyRegister :: Int -> LWord -> SystemState
modifyRegister idx value = do
    (PD32 a cpuRegisters b c d) <- get
    let registers' = changeSelectedRegister cpuRegisters
    put (PD32 a registers' b c d)
    where
        changeSelectedRegister rs =
            take idx rs ++ [value] ++ drop (idx + 1) rs

setPC :: LWord -> SystemState
setPC value = do
    (PD32 ctrlUnit a b c d) <- get
    let (ControlUnit _ ir mbr mar size clock) = ctrlUnit
        ctrlUnit' = ControlUnit value ir mbr mar size clock
    put (PD32 ctrlUnit' a b c d)

--readMemory :: Address -> SystemState
--readMemory addr = do
    --(PD32 ctrlUnit a b memory d) <- get
    --let 
