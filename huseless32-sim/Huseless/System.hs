module Huseless.System
where

import Control.Applicative ((<$>))
import Data.Bits (zeroBits, bit, setBit, testBit)
import Control.Monad (sequence_)
import Control.Monad.Except
import Control.Monad.State.Lazy
import Data.Int (Int8, Int16, Int32)

import Huseless.Common
import Huseless.Instruction
import Huseless.System.Memory.VectorMemory
import Huseless.System.PD32
import Huseless.System.ALUOps
import Huseless.System.StatusRegister

data Failure = Trace | HaltExecution | InvalidMemoryAddress | InvalidOpCode
  deriving (Eq, Show)

type System m a = ExceptT Failure (StateT PD32 m) a

-- TODO: check register index
readRegister :: Monad m => Int -> System m LWord
readRegister idx = do
    pd32 <- get
    return $ (cpuRegisters pd32) !! idx

writeRegister :: Monad m => Int -> LWord -> System m ()
writeRegister idx value = do
    pd32 <- get
    let registers' = changeSelectedRegister . cpuRegisters $ pd32
    put pd32 { cpuRegisters = registers' }
    where
        changeSelectedRegister rs =
            take idx rs ++ [value] ++ drop (idx + 1) rs


readMemory :: Monad m => Z -> Address -> System m LWord
readMemory z addr = do
    pd32 <- get
    let mem = memory pd32

    case readAligned mem z addr  of
      Nothing -> throwError InvalidMemoryAddress
      Just value -> return value

writeMemory :: Monad m => Z -> Address -> LWord -> System m ()
writeMemory z addr value = do
    pd32 <- get
    let mem = memory pd32

    case writeAligned mem z addr value of
      Nothing -> throwError InvalidMemoryAddress
      Just newMemory -> put pd32 {
          memory = newMemory
          }

readAtPCAndIncrement :: Monad m => System m LWord
readAtPCAndIncrement = do
    pd32 <- get
    let oldPc = pc pd32
    put pd32 { pc = oldPc + 4 }
    readMemory ZLWord oldPc

getExtensionLWord :: Monad m => System m LWord
getExtensionLWord = readAtPCAndIncrement

extendSign :: Z -> LWord -> LWord
extendSign ZByte  = fromIntegral . (fromIntegral :: LWord -> Int8)
extendSign ZWord  = fromIntegral . (fromIntegral :: LWord -> Int16)
extendSign ZLWord = fromIntegral . (fromIntegral :: LWord -> Int32) -- identity

needsSignExtension :: Instruction -> Bool
needsSignExtension instr =
    case destAMode instr of
      AMRegister _ -> True
      _ -> False

doSignExtension :: Monad m => Instruction -> LWord -> System m LWord
doSignExtension instr = return . if needsSignExtension instr
                                    then extendSign (size instr)
                                    else id

readSource :: Monad m => AddressingMode -> Z -> System m LWord
readSource addrMode z = do
    pd32 <- get

    result <- case addrMode of
        AMRegister reg -> readRegister reg
        AMImmediate -> do
            value <- getExtensionLWord
            return value
        AMAbsolute -> do
            address <- getExtensionLWord
            readMemory z address
        AMIndirectRegister reg ->
            readRegister reg >>=
            readMemory z
        AMRegisterOffset reg -> do
            offset <- getExtensionLWord
            base <- readRegister reg
            readMemory z $ base + (byteSize z) * offset
        AMRelativeOffset -> do
            let oldPc = pc pd32
            offset <- getExtensionLWord
            readMemory z $ oldPc + (byteSize ZLWord) * offset
        AMIncrementing reg -> do
            address <- readRegister reg
            writeRegister reg $ address + byteSize z
            readMemory z address
        AMDecrementing reg -> do
            regValue <- readRegister reg
            let address = regValue - byteSize z
            writeRegister reg address
            readMemory z address

    return $ mask z result

writeDest :: Monad m => AddressingMode -> Z -> LWord -> System m ()
writeDest addrMode z unmaskedValue = do
    pd32 <- get
    let value = mask z unmaskedValue

    case addrMode of
        AMRegister reg -> writeRegister reg value
        AMImmediate -> error "writing to an immediate value"
        AMAbsolute -> do
            address <- getExtensionLWord
            writeMemory z address value
        AMIndirectRegister reg -> do
            address <- readRegister reg
            writeMemory z address value
        AMRegisterOffset reg -> do
            offset <- getExtensionLWord
            base <- readRegister reg
            writeMemory z (base + (byteSize z) * offset) value
        AMRelativeOffset -> do
            let oldPc = pc pd32
            offset <- getExtensionLWord
            writeMemory z (oldPc + (byteSize ZLWord) * offset) value
        AMIncrementing reg -> do
            address <- readRegister reg
            writeRegister reg (address + byteSize z)
            writeMemory z address value
        AMDecrementing reg -> do
            regValue <- readRegister reg
            let address = regValue - (byteSize z)
            writeRegister reg address
            writeMemory z address value

modifySR :: Monad m => (StatusRegister -> StatusRegister) -> System m ()
modifySR modify = do
    pd32 <- get
    let statusRegister' = modify $ statusRegister pd32
    put pd32 { statusRegister = statusRegister' }

setCarry value = modifySR (\sr -> sr {carry = value})
setNegative value = modifySR (\sr -> sr {negative = value})
setZero value = modifySR (\sr -> sr {zero = value})
setOverflow value = modifySR (\sr -> sr {overflow = value})
setParity value = modifySR (\sr -> sr {parity = value})
setInterrupt value = modifySR (\sr -> sr {interrupt = value})

readSR :: Monad m => System m LWord
readSR = do
    pd32 <- get
    let (StatusRegister c n z o p i) = statusRegister pd32
        indeces = map snd $ filter fst $ zip [c, n, z, o, p, i] [0..] :: [Int]

    return $ foldl setBit zeroBits indeces

writeSR :: Monad m => LWord -> System m ()
writeSR word = sequence_ $
    zipWith ($) -- TODO: find better approach, maybe with Applicative over StatusRegister
        [setCarry, setNegative, setZero, setOverflow, setParity, setInterrupt]
        [testBit word i | i <- [0..]]


aluCompute :: Monad m =>
              (LWord -> LWord -> LWord) ->
              LWord ->
              LWord -> -- value of destination register
              System m LWord
aluCompute op value1 value2 =
    let result = op value1 value2
        (c, n, z, o, p) = standardConditionsOn value2 result
     in do
        setCarry c
        setNegative n
        setZero z
        setOverflow o
        setParity p

        return result
