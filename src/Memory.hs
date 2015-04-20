{-# LANGUAGE MultiParamTypeClasses #-}
module Memory
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
    checkAddress :: Address -> a -> Maybe b

