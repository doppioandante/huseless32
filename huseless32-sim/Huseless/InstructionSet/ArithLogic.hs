module Huseless.InstructionSet.ArithLogic where

import Control.Monad.Except
import Control.Monad.State
import Data.Bits ((.&.), (.|.), xor, complement)
import Data.Int (Int16, Int32, Int64)
import Data.Word (Word64)

import Huseless.Common
import Huseless.Instruction
import Huseless.InstructionSet.Validation
import Huseless.System
import Huseless.System.ALUOps
import Huseless.System.PD32
import Huseless.System.StatusRegister

arithmeticInstructionTable :: Monad m => [(Int, Instruction -> System m ())]
arithmeticInstructionTable = [
    (0, opADD),
    (1, opADC),
    (2, opCMP),
    (3, opNEG),
    (4, opSUB),
    (5, opSBB),
    (6, opSMUL),
    (7, opUMUL),
    (8, opSDIV),
    (9, opUDIV),
    (10, opDSMUL),
    (11, opDUMUL),
    (12, opDSDIV),
    (13, opDUDIV)
    ]

logicInstructionTable :: Monad m => [(Int, Instruction -> System m ())]
logicInstructionTable = [
    (0, opAND),
    (1, opOR),
    (2, opXOR),
    (3, opNOT)
    ]

sizedMatcher = defaultMatcher `matchSize` mAll `matchSM` mAll `matchDM` mRegister

aluOpHeader :: Monad m => Instruction -> System m (Z, Int, LWord, LWord)
aluOpHeader instr = do
    (_, _, size, sourceAMode, reg) <- match instr sizedMatcher
    s <- readSource sourceAMode size
    r <- readRegister reg
    return (size, reg, s, r)

opADD :: Monad m => Instruction -> System m ()
opADD instr = do
    (size, reg, s, r) <- aluOpHeader instr
    res <- aluCompute (+) (extendSign size r) (extendSign size s)
    writeRegister reg (mask size res)

boolToNum :: Num a => Bool -> a
boolToNum b = if b then 1 else 0

opADC :: Monad m => Instruction -> System m ()
opADC instr = do
    (size, reg, s, r) <- aluOpHeader instr
    pd32 <- get
    let c = carry . statusRegister $ pd32
        carrySum a b = (a+b) + boolToNum c
    res <- aluCompute carrySum (extendSign size r) (extendSign size s)
    writeRegister reg (mask size res)

opCMP :: Monad m => Instruction -> System m ()
opCMP instr = do
    (size, reg, s, r) <- aluOpHeader instr
    aluCompute (-) (extendSign size r) (extendSign size s)
    return ()
    -- aluCompute will set the flags

opNEG :: Monad m => Instruction -> System m ()
opNEG instr = do
    (size, reg, s, r) <- aluOpHeader instr
    res <- aluCompute (-) 0 (extendSign size s)
    writeRegister reg (mask size res)

opSUB :: Monad m => Instruction -> System m ()
opSUB instr = do
    (size, reg, s, r) <- aluOpHeader instr
    res <- aluCompute (-) (extendSign size r) (extendSign size s)
    writeRegister reg (mask size res)

opSBB :: Monad m => Instruction -> System m ()
opSBB instr = do
    (size, reg, s, r) <- aluOpHeader instr
    pd32 <- get
    let c = carry . statusRegister $ pd32
        carrySub a b = (a-b) - boolToNum (not c)
    res <- aluCompute carrySub (extendSign size r) (extendSign size s)
    writeRegister reg (mask size res)


unsizedMatcher = defaultMatcher `matchSM` mAll `matchDM` mRegister

opSMUL :: Monad m => Instruction -> System m ()
opSMUL instr = do
    (_, _, _, sourceAMode, reg) <- match instr unsizedMatcher
    s <- readSource sourceAMode ZWord
    r <- readRegister reg
    res <- aluCompute signedWordMul (mask ZWord r) s
    writeRegister reg res
  where
      signedWordMul x y =
          let x' = fromIntegral x :: Int16
              y' = fromIntegral y :: Int16
          in fromIntegral (x*y) :: LWord


opUMUL :: Monad m => Instruction -> System m ()
opUMUL instr = do
    (_, _, _, sourceAMode, reg) <- match instr unsizedMatcher
    s <- readSource sourceAMode ZWord
    r <- readRegister reg
    res <- aluCompute (*) (mask ZWord r) s
    writeRegister reg res

opSDIV :: Monad m => Instruction -> System m ()
opSDIV instr = do
    (_, _, _, sourceAMode, reg) <- match instr unsizedMatcher
    s <- readSource sourceAMode ZWord
    r <- readRegister reg
    res <- aluCompute signedDiv r (mask ZWord s)
    writeRegister reg res
    setCarry False
  where
      signedDiv x y =
          let x' = fromIntegral x :: Int32
              y' = fromIntegral (fromIntegral y :: Int16) :: Int32
              q' = fromIntegral $ x' `div` y'
              r' = fromIntegral $ x' `mod` y'
          in fromIntegral $ r'*0x10000 + q' -- r' and q' are written in sequence
                                            -- in the 32bit register
opUDIV :: Monad m => Instruction -> System m ()
opUDIV instr = do
    (_, _, _, sourceAMode, reg) <- match instr unsizedMatcher
    s <- readSource sourceAMode ZWord
    r <- readRegister reg
    res <- aluCompute unsignedDiv r (mask ZWord s)
    writeRegister reg res
    setCarry False
  where
      unsignedDiv x y =
          let q = fromIntegral $ x `div` y
              r = fromIntegral $ x `mod` y
          in fromIntegral $ r*0x10000 + q

opDSMUL :: Monad m => Instruction -> System m ()
opDSMUL instr = do
    (_, _, _, sourceAMode, reg) <- match instr unsizedMatcher
    if reg > 5
       then throwError InvalidOpCode
       else return ()

    s <- readSource sourceAMode ZLWord
    r <- readRegister reg
    m1 <- aluCompute (signedLWordMul 0) r s
    m2 <- aluCompute (signedLWordMul 1) r s
    writeRegister reg m1
    writeRegister (reg+1) m2
  where
      signedLWordMul part x y =
          let x' = fromIntegral (fromIntegral x :: Int32) :: Int64
              y' = fromIntegral (fromIntegral y :: Int32) :: Int64
          in if part == 0
                then fromIntegral $ (x' * y') `mod` 0x100000000
                else fromIntegral $ (x' * y') `div` 0x100000000

opDUMUL :: Monad m => Instruction -> System m ()
opDUMUL instr = do
    (_, _, _, sourceAMode, reg) <- match instr unsizedMatcher
    if reg > 5
       then throwError InvalidOpCode
       else return ()

    s <- readSource sourceAMode ZLWord
    r <- readRegister reg
    m1 <- aluCompute (unsignedLWordMul 0) r s
    m2 <- aluCompute (unsignedLWordMul 1) r s
    writeRegister reg m1
    writeRegister (reg+1) m2
  where
      unsignedLWordMul part x y =
          let x' = fromIntegral x :: Word64
              y' = fromIntegral y :: Word64
          in if part == 0
                then fromIntegral $ (x' * y') `mod` 0x100000000
                else fromIntegral $ (x' * y') `div` 0x100000000

opDSDIV :: Monad m => Instruction -> System m ()
opDSDIV instr = do
    (_, _, _, sourceAMode, reg) <- match instr unsizedMatcher
    if reg > 5
       then throwError InvalidOpCode
       else return ()

    s <- readSource sourceAMode ZLWord
    r <- readRegister reg
    quotient <- aluCompute (signedLWordDiv 0) r s
    remainder <- aluCompute (signedLWordDiv 1) r s
    writeRegister reg quotient
    writeRegister (reg+1) remainder
  where
      signedLWordDiv part x y =
          let x' = fromIntegral x :: Int32
              y' = fromIntegral y :: Int32
          in if part == 0
                then fromIntegral $ (x' `div` y')
                else fromIntegral $ (x' `mod` y')

opDUDIV :: Monad m => Instruction -> System m ()
opDUDIV instr = do
    (_, _, _, sourceAMode, reg) <- match instr unsizedMatcher
    if reg > 5
       then throwError InvalidOpCode
       else return ()

    s <- readSource sourceAMode ZLWord
    r <- readRegister reg
    quotient <- aluCompute (unsignedLWordDiv 0) r s
    remainder <- aluCompute (unsignedLWordDiv 1) r s
    writeRegister reg quotient
    writeRegister (reg+1) remainder
  where
      unsignedLWordDiv part x y =
          if part == 0
             then fromIntegral $ (x `div` y)
             else fromIntegral $ (x `mod` y)



opAND :: Monad m => Instruction -> System m ()
opAND instr = do
    (size, reg, s, r) <- aluOpHeader instr
    res <- aluCompute (.&.) r s
    writeRegister reg res -- significat word is zero, no need to mask

opOR :: Monad m => Instruction -> System m ()
opOR instr = do
    (size, reg, s, r) <- aluOpHeader instr
    res <- aluCompute (.|.) r s
    writeRegister reg res -- significat word is zero, no need to mask

opXOR :: Monad m => Instruction -> System m ()
opXOR instr = do
    (size, reg, s, r) <- aluOpHeader instr
    res <- aluCompute (xor) r s
    writeRegister reg res -- significat word is zero, no need to mask

opNOT :: Monad m => Instruction -> System m ()
opNOT instr = do
    (size, reg, s, r) <- aluOpHeader instr
    -- WARNING: HACK YOLO
    res <- aluCompute const (complement s) 0
    writeRegister reg (mask size res)

