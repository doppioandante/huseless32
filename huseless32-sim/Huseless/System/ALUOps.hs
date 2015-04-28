module Huseless.System.ALUOps where

import Data.Bits (testBit, finiteBitSize, xor)

import Huseless.Common
import Huseless.System.StatusRegister


lwordBits :: Int
lwordBits = finiteBitSize (0 :: LWord)

getSignBit :: LWord -> Bool
getSignBit word = testBit word (lwordBits - 1)

standardConditionsOn :: LWord -> LWord -> (Bool, Bool, Bool, Bool, Bool)
standardConditionsOn original result =
    let carry = result < original
        negative = getSignBit result
        zero = result == 0
        overflow = (getSignBit original) /= (getSignBit result)
        parity = foldl xor True
                       [testBit result i | i <- [0..(lwordBits - 1)]]
    in
        (carry, negative, zero, overflow, parity)


