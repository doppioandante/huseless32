module Common
(
Word8,
LWord,
Address,
Z(..),
mask,
byteSize
)
where

import Data.Word (Word8, Word16, Word32)
import Data.Bits ((.&.))

type LWord = Word32
type Address = LWord

data Z = ZByte | ZWord | ZLWord deriving (Eq, Show, Read)

mask ZByte  = fromIntegral . (fromIntegral :: LWord -> Word8)
mask ZWord  = fromIntegral . (fromIntegral :: LWord -> Word16)
mask ZLWord = fromIntegral . (fromIntegral :: LWord -> Word32) -- identity

byteSize :: Num a => Z -> a
byteSize z = case z of
               ZByte -> 1
               ZWord -> 2
               ZLWord -> 4

--data Register = R0, R1, R2, R3, R4, R5, R6, R7



