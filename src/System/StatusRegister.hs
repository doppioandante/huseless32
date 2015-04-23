module System.StatusRegister where

data StatusRegister = StatusRegister
    {
        carry :: Bool,
        negative :: Bool,
        zero :: Bool,
        overflow :: Bool,
        parity :: Bool,
        interrupt :: Bool
    }
    deriving (Eq, Show, Read)

