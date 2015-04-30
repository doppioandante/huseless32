module Huseless.InstructionSet.Validation where

import Data.Maybe
import Data.Word (Word8)
import Control.Monad.Except (throwError)

import Huseless.Instruction
import Huseless.System

data Matcher a b c d = Matcher
    {
        mExt1 :: Word8 -> Maybe a,
        mExt2 :: Word8 -> Maybe b,
        mSize :: Z -> Maybe Z,
        mSM   :: AddressingMode -> Maybe c,
        mDM   :: AddressingMode -> Maybe d
    }

match :: Monad m =>
         Instruction ->
         Matcher a b c d ->
         System m (a, b, Z, c, d)
match instr matcher =
    let Instruction _ _ ext1 ext2 size sM dM = instr
        Matcher f1 f2 f3 f4 f5 = matcher
        tryMatch = do
                    res1 <- f1 ext1
                    res2 <- f2 ext2
                    res3 <- f3 size
                    res4 <- f4 sM
                    res5 <- f5 dM
                    return (res1, res2, res3, res4, res5)
     in case tryMatch of
            Just x  -> return x
            Nothing -> throwError InvalidOpCode

valid :: Monad m => Instruction -> Matcher a b c d -> System m ()
valid instr matcher = do
    match instr matcher
    return ()

defaultMatcher :: Matcher Word8 Word8 AddressingMode AddressingMode
defaultMatcher = Matcher
    {
        mExt1 = mExact 0,
        mExt2 = mExact 0,
        mSize = mExact ZByte,
        mSM   = mExact $ AMRegister 0,
        mDM   = mExact $ AMRegister 0
    }

infixl `matchExt1`
matchExt1 :: Matcher a b c d -> (Word8 -> Maybe e) -> Matcher e b c d
matchExt1 (Matcher _ mExt2' mSize' mSM' mDM') f =
    Matcher f mExt2' mSize' mSM' mDM'

infixl `matchExt2`
matchExt2 :: Matcher a b c d -> (Word8 -> Maybe e) -> Matcher a e c d
matchExt2 (Matcher mExt1' _ mSize' mSM' mDM') f =
    Matcher mExt1' f mSize' mSM' mDM'

infixl `matchSize`
matchSize :: Matcher a b c d -> (Z -> Maybe Z) -> Matcher a b c d
matchSize (Matcher mExt1' mExt2' _ mSM' mDM') f =
    Matcher mExt1' mExt2' f mSM' mDM'

infixl `matchSM`
matchSM :: Matcher a b c d -> (AddressingMode -> Maybe e) -> Matcher a b e d
matchSM (Matcher mExt1' mExt2' mSize' _ mDM') f =
    Matcher mExt1' mExt2' mSize' f mDM'

infixl `matchDM`
matchDM :: Matcher a b c d -> (AddressingMode -> Maybe e) -> Matcher a b c e
matchDM (Matcher mExt1' mExt2' mSize' mSM' _) f =
    Matcher mExt1' mExt2' mSize' mSM' f


mExact :: Eq a => a -> a -> Maybe a
mExact expected value = if expected == value
                               then Just value
                               else Nothing

mRegister :: AddressingMode -> Maybe Int
mRegister mode =
    case mode of
      AMRegister idx -> Just idx
      _              -> Nothing

mAll :: a -> Maybe a
mAll = Just
