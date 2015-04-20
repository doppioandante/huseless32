module Common
(
Word8,
LWord,
Address,
Register
)
where

import Data.Word (Word8, Word32)

type LWord = Word32
type Address = LWord
type Register = LWord



