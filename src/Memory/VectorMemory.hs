{-# LANGUAGE MultiParamTypeClasses #-}
module Memory.VectorMemory
(
VectorMemory,
readWord,
writeWord,
checkAddress,
emptyVectorMemory
)
where

import qualified Data.Vector as V

import Common
import Memory

newtype VectorMemory = VectorMemory (V.Vector LWord) deriving (Eq, Show)

instance RandomAccessible VectorMemory Int where
    readWord (VectorMemory v) index = v V.! index
    writeWord (VectorMemory v) index word = VectorMemory $ v V.// [(index, word)]
    checkAddress (VectorMemory v) address =
        if address `mod` 4 == 0 && index <= (V.length v)
            then Just index
            else Nothing
        where
            index = (fromIntegral address) `div` 4

emptyVectorMemory :: Int -> VectorMemory
emptyVectorMemory size = VectorMemory (V.replicate size 0)
