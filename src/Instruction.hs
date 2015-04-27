module Instruction
(
Instruction(..),
Z(..),
AddressingMode(..),
decodeInstruction
)
where

import Data.Word (Word8, Word32)
import Data.Binary (encode)
import Data.Binary.Get (runGet)
import Data.Binary.Bits.Get (runBitGet, block, word8)

import Common

data RawInstruction = RawInstruction
    {
        rawClass :: Word8, -- C
        rawCode :: Word8, -- I
        a1 :: Word8, -- extra instruction data
        a2 :: Word8, -- extra instruction data
        z :: Word8, -- encoded size of operands
        sM :: Word8,
        sR :: Word8,
        dM :: Word8,
        dR :: Word8
    }
    deriving (Eq, Show)

data AddressingMode =
    AMRegister Int |
    AMImmediate |
    AMAbsolute |
    AMIndirectRegister Int |
    AMRegisterOffset Int |
    AMRelativeOffset |
    AMIncrementing Int |
    AMDecrementing Int
    deriving (Eq, Show, Read)


data Instruction = Instruction
    {
        iClass :: Int,
        iCode :: Int,
        ext1 :: Word8,
        ext2 :: Word8,
        size :: Z,
        sourceAMode :: AddressingMode,
        destAMode :: AddressingMode
    }
    deriving (Eq, Show, Read)

decodeInstruction :: LWord -> Instruction
decodeInstruction op =
    decodeRawInstruction . (`runGet` (encode op)) . runBitGet . block $
        RawInstruction <$> word8 3
                    <*> word8 5
                    <*> word8 8
                    <*> word8 2
                    <*> word8 2
                    <*> word8 3
                    <*> word8 3
                    <*> word8 3
                    <*> word8 3

decodeRawInstruction :: RawInstruction -> Instruction
decodeRawInstruction =
    Instruction <$> decodeClass . rawClass
                <*> decodeInstr . rawCode
                <*> a1
                <*> a2
                <*> decodeZ . z
                <*> (decodeMode <$> sM <*> sR)
                <*> (decodeMode <$> dM <*> dR)

decodeClass = fromIntegral :: Word8 -> Int
decodeInstr = fromIntegral :: Word8 -> Int
decodeZ bits =
    case bits of
      0 -> ZByte
      1 -> ZWord
      2 -> ZLWord

decodeMode addressing reg =
    let reg' = fromIntegral reg :: Int
    in case addressing of
      0 -> AMRegister reg'
      1 -> AMImmediate
      2 -> AMAbsolute
      3 -> AMIndirectRegister reg'
      4 -> AMRegisterOffset reg'
      5 -> AMRelativeOffset
      6 -> AMIncrementing reg'
      7 -> AMDecrementing reg'
