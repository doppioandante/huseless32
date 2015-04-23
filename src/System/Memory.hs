{-# LANGUAGE MultiParamTypeClasses #-}
module System.Memory
(
RandomAccessible,
readWord,
writeWord,
checkAddress
)
where

import Data.Maybe

import Common

-- a: Memory type
-- b: Address type
class RandomAccessible a b where
    readWord :: a -> b -> LWord
    writeWord :: a -> b -> LWord -> a
    checkAddress :: a -> Address -> Maybe b

