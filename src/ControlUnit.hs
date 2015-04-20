module ControlUnit
where

import Data.Bits ((.&.))

import Common

data Z = ZByte | ZWord | ZLWord deriving (Eq, Show, Read)

data ControlUnit = ControlUnit
    {
        pc   :: Register, -- program counter
        ir   :: Register, -- instruction register
        mbr  :: Register, -- memory bus register
        mar  :: Register, -- memory address register
        size :: Z, -- Z signal determines the size of the operands

        clock :: Double
    }
    deriving (Eq, Show)

maskZ :: Z -> LWord -> LWord
maskZ z = case z of
    ZByte -> (.&. 0x000000FF)
    ZWord -> (.&. 0x0000FFFF)
    ZLWord -> id
