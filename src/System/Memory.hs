module System.Memory
(
RandomAccessible,
readAligned,
writeAligned,
)
where

import Data.Maybe

import Common

-- a: Memory type
-- b: Address type
class RandomAccessible a where
    readAligned :: a -> Z -> Address -> Maybe LWord
    writeAligned :: a -> Z -> Address -> LWord -> Maybe a

