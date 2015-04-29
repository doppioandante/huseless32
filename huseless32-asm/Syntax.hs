module Syntax where

data AsmLine = Instruction Label InstructionBody
             | PseudoInstruction Label PseudoInstructionBody
             | Assignment String Expression
             deriving (Eq, Show)

data Label = Label String
           | EmptyLabel
           deriving (Eq, Show)

data InstructionBody = Mnemonic0 String -- HALT, NOP, RESET, TRAP, TRACE,...
                     | Mnemonic1 String Operand -- MOVTOSTR, PUSH, POP..
                     | Mnemonic2 String Operand Operand -- MOV, MVL
                     deriving (Eq, Show)

data PseudoInstructionBody = Declare Size Expression
                           | Initialize Size InitList
    deriving (Eq, Show)

data Expression = ExprValue Integer
                | Variable String
                | BinaryOp OpType Expression Expression
                deriving (Eq, Show)

data OpType = Plus
            | Minus
            | Multiply
            deriving (Eq, Show)

data Operand = Direct    Register
             | Immediate Expression
             | Absolute  Expression
             | Indirect  Register
             | IndirectOffset Expression Register
             | RelativeOffset Expression
             | Decrementing   Register
             | Incrementing   Register
             | Device Expression
             deriving (Eq, Show)

data Size = SizeByte | SizeWord | SizeLWord deriving (Eq, Show)

data InitList = InitList InitValue [InitValue]
              deriving (Eq, Show)

data InitValue = InitASCII String
               | InitExpr  Expression
               deriving (Eq, Show)

data Register = Register Int deriving (Eq, Show)

