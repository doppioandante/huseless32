{-# LANGUAGE ExistentialQuantification #-}
module Huseless.System.PD32 where

import Huseless.Common
import Huseless.System.Device
import Huseless.System.Memory
import Huseless.System.StatusRegister

type LegalAddress = Int

data  AnyMem = forall mem . RandomAccessible mem => AnyMem mem

instance RandomAccessible AnyMem where
    readAligned (AnyMem m) z address = readAligned m z address

    writeAligned (AnyMem m) z address value =
        AnyMem <$> writeAligned m z address value

data PD32 = PD32
    {
        pc   :: LWord, -- program counter

        cpuRegisters :: [LWord],
        statusRegister :: StatusRegister,
        memory :: AnyMem,
        devices :: [Device]
    }




