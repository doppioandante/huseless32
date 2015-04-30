{-# LANGUAGE ExistentialQuantification #-}

module Huseless.System.Memory
(
RandomAccessible,
readAligned,
writeAligned,
AnyMem(..)
)
where

import Data.Maybe

import Huseless.Common

class RandomAccessible a where
    readAligned :: a -> Z -> Address -> Maybe LWord
    writeAligned :: a -> Z -> Address -> LWord -> Maybe a

data  AnyMem = forall mem . RandomAccessible mem => AnyMem mem

instance RandomAccessible AnyMem where
    readAligned (AnyMem m) z address = readAligned m z address

    writeAligned (AnyMem m) z address value =
        AnyMem <$> writeAligned m z address value
