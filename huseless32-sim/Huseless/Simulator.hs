module Huseless.Simulator where

import Control.Monad.Except
import Control.Monad.State.Lazy

import Huseless.Common
import Huseless.Instruction
import Huseless.InstructionSet
import Huseless.System
import Huseless.System.PD32

runSystem :: Monad m =>
             PD32 ->
             m (Failure, PD32)
runSystem currentPD32 = do
    (result, newPD32) <- (runStateT $ runExceptT fetchDecodeExecute) $
                         currentPD32
    case result of
      Right () -> runSystem newPD32
      Left failure -> return (failure, newPD32)

fetchDecodeExecute :: Monad m => System m ()
fetchDecodeExecute = do
   encodedInstruction <- readAtPCAndIncrement
   runInstruction (encodedInstruction)

runInstruction :: Monad m => LWord -> System m ()
runInstruction lword =
    let instr = decodeInstruction lword
        cl    = iClass instr
        code  = iCode instr
    in case getAction cl code of
         Nothing -> throwError InvalidOpCode
         Just action -> action instr
