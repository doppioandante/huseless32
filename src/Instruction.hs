module Instruction
(
Instruction(..),
decodeInstruction
)
where

import Data.Word (Word8, Word32)
import Data.Binary (encode)
import Data.Binary.Get (runGet)
import Data.Binary.Bits.Get (runBitGet, block, word8)
import Data.Bits ((.&.))

import Common

data Instruction = Instruction
    {
        instrClass :: Word8, -- C
        instrCode :: Word8, -- I
        a1 :: Word8, -- extra instruction data
        a2 :: Word8, -- extra instruction data
        z :: Word8, -- encoded size of operands
        sourceAddressing :: Word8,
        sourceRegister :: Word8,
        destAddressing :: Word8,
        destRegister :: Word8
    }
    deriving (Eq, Show)

decodeInstruction :: LWord -> Instruction
decodeInstruction op =
    (`runGet` (encode op)) . runBitGet . block $
        Instruction <$> word8 3
                    <*> word8 5
                    <*> word8 8
                    <*> word8 2
                    <*> word8 2
                    <*> word8 3
                    <*> word8 3
                    <*> word8 3
                    <*> word8 3
