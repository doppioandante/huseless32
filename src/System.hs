module System
where

import Control.Applicative ((<$>))
import Data.Bits ((.&.))
import Control.Monad (liftM)
import Control.Monad.Except
import Control.Monad.State.Lazy
import Data.Int (Int8, Int16, Int32)

import Common
import Instruction
import Memory.VectorMemory
import PD32

data Failure = Trace | HaltExecution | InvalidMemoryAddress | InvalidOpCode
  deriving (Eq, Show)

type System m a = ExceptT Failure (StateT PD32 m) a

-- TODO: check register index
readRegister :: Int -> PD32 -> LWord
readRegister idx pd32 = (cpuRegisters pd32) !! idx

writeRegister :: Monad m => Int -> LWord -> System m ()
writeRegister idx value = do
    pd32 <- get
    let registers' = changeSelectedRegister . cpuRegisters $ pd32
    put pd32 { cpuRegisters = registers' }
    where
        changeSelectedRegister rs =
            take idx rs ++ [value] ++ drop (idx + 1) rs

readMemory :: Monad m => Address -> System m LWord
readMemory addr = do
    pd32 <- get
    let mem = memory pd32

    case checkAddress mem addr of
      Nothing -> throwError InvalidMemoryAddress
      Just idx -> return $ readWord mem (idx::LegalAddress)

writeMemory :: Monad m => Address -> LWord -> System m ()
writeMemory addr value = do
    pd32 <- get
    let mem = memory pd32

    case checkAddress mem addr of
      Nothing -> throwError InvalidMemoryAddress
      Just idx -> put pd32 {
          memory = writeWord mem (idx::LegalAddress) value
          }

readAtPCAndIncrement :: Monad m => System m LWord
readAtPCAndIncrement = do
    pd32 <- get
    let oldPc = pc pd32
    put pd32 { pc = oldPc + 4 }
    readMemory oldPc

getExtensionLWord :: Monad m => System m LWord
getExtensionLWord = readAtPCAndIncrement

mask :: Z -> LWord -> LWord
mask z = case z of
              ZByte -> (.&. 0x000000FF)
              ZWord -> (.&. 0x0000FFFF)
              ZLWord -> id

byteSize :: Z -> LWord
byteSize z = case z of
               ZByte -> 1
               ZWord -> 2
               ZLWord -> 4

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
        AMRegister reg -> return $ readRegister reg pd32
        AMImmediate -> do
            value <- getExtensionLWord
            return value
        AMAbsolute -> do
            address <- getExtensionLWord
            readMemory address
        AMIndirectRegister reg -> do
            let address = readRegister reg pd32
            readMemory address
        AMRegisterOffset reg -> do
            offset <- getExtensionLWord
            readMemory $ (readRegister reg pd32) + (byteSize z) * offset
        AMRelativeOffset -> do
            let oldPc = pc pd32
            offset <- getExtensionLWord
            readMemory $ oldPc + (byteSize ZLWord) * offset
        AMIncrementing reg -> do
            let address = readRegister reg pd32
            writeRegister reg (address + byteSize z)
            readMemory address
        AMDecrementing reg -> do
            let address = (readRegister reg pd32) - byteSize z
            writeRegister reg address
            readMemory address

    return $ mask z result

writeDest :: Monad m => AddressingMode -> Z -> LWord -> System m ()
writeDest addrMode z unmaskedValue = do
    pd32 <- get
    let value = mask z unmaskedValue

    case addrMode of
        AMRegister reg -> writeRegister reg value
        AMImmediate -> throwError InvalidOpCode
        AMAbsolute -> do
            address <- getExtensionLWord
            writeMemory address value
        AMIndirectRegister reg -> do
            let address = readRegister reg pd32
            writeMemory address value
        AMRegisterOffset reg -> do
            offset <- getExtensionLWord
            writeMemory ((readRegister reg pd32) + (byteSize z) * offset) value
        AMRelativeOffset -> do
            let oldPc = pc pd32
            offset <- getExtensionLWord
            writeMemory (oldPc + (byteSize ZLWord) * offset) value
        AMIncrementing reg -> do
            let address = readRegister reg pd32
            writeRegister reg (address + byteSize z)
            writeMemory address value
        AMDecrementing reg -> do
            let address = (readRegister reg pd32) - byteSize z
            writeRegister reg address
            writeMemory address value

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

