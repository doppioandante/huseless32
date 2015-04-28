module Huseless.System.Memory.VectorMemory
(
VectorMemory,
readAligned,
writeAligned,
emptyVectorMemory
)
where

import Data.Binary (encode, decode)
import qualified Data.ByteString.Lazy as BS (take, drop, append)
import Data.Int (Int64)
import qualified Data.Vector as V

import Huseless.Common
import Huseless.System.Memory

newtype VectorMemory = VectorMemory (V.Vector LWord) deriving (Eq, Show)

instance RandomAccessible VectorMemory where
    readAligned (VectorMemory mem) z address =
        getIndex mem (byteSize z) address >>=
        return . (placeBytes z byteIdx 0) . (mem V.!)
          where
              byteIdx = case z of -- TODO: fix endianness?
                          ZByte -> 3
                          ZWord -> 2
                          ZLWord -> 0

    writeAligned (VectorMemory mem) z address word = do
        index <- getIndex mem (byteSize z) address
        let byteIdx = fromIntegral $ address `mod` byteSize z
            oldValue = mem V.! index
        return $ VectorMemory $
          mem V.// [(index, placeBytes z byteIdx oldValue word)]

getIndex mem alignment address =
    -- we need to access memory in chunks of 4 bytes
    -- but the address can be aligned to lower sizes (2 bytes or byte)
    let index = fromIntegral $ address `div` (byteSize ZLWord) :: Int
    in if address `mod` alignment  == 0 && index <= (V.length mem)
          then Just index
          else Nothing

placeBytes :: Z -> Int64 -> LWord -> LWord -> LWord
placeBytes z byteIdx base bytes =
    let size = fromIntegral $ byteSize z
        pre = BS.take byteIdx $ encode base
        post = (BS.drop (byteIdx + size)) $ encode base
        middle = BS.drop (byteSize ZLWord - size) $ encode bytes
    in
        decode $ pre `BS.append` middle `BS.append` post

emptyVectorMemory :: Int -> VectorMemory
emptyVectorMemory size = VectorMemory (V.replicate size 0)
