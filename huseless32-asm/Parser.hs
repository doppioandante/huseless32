module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Expr

import Lexer
import Syntax

binary s f = Expr.Infix (reservedOp s >> return (BinaryOp f)) Expr.AssocLeft

table = [[binary "*" Multiply]
        ,[binary "+" Plus,
          binary "-" Minus]]

expression :: Parser Expression
expression = Expr.buildExpressionParser table factor

variable ::  Parser Expression
variable = Variable <$> identifier

exprInteger :: Parser Expression
exprInteger = ExprValue <$> integer

factor :: Parser Expression
factor = exprInteger <|> variable <|> parens expression

assignment :: Parser AsmStmt
assignment = Assignment <$> identifier <* reservedOp "=" <*> expression

asmLabel :: Parser Label
asmLabel = option EmptyLabel (
               Label <$> identifier <* reservedOp ":"
           )

 -- TODO: is this efficient? Maybe lazyness is saving us
findReserved table =
    foldr1 (<|>) $
      (flip map) table $ \instr ->
        try $ reserved instr >> return instr

mnemonic0 :: Parser InstructionBody
mnemonic0 = do
    name <- findReserved mnemonic0Names
    -- TODO: check validity early
    return $ Mnemonic0 name

mnemonic1 :: Parser InstructionBody
mnemonic1 = do
    name <- findReserved mnemonic1Names
    op <- operand
    -- TODO: check validity early
    return $ Mnemonic1 name op

mnemonic2 :: Parser InstructionBody
mnemonic2 = do
    name <- findReserved mnemonic2Names
    op1 <- operand
    symbol ","
    op2 <- operand
    -- TODO: check validity early
    return $ Mnemonic2 name op1 op2

operand :: Parser Operand
operand =  try direct
       <|> try immediate
       <|> try absolute
       <|> try indirect
       <|> try indirectOffset
       <|> try relativeOffset
       <|> try decrementing
       <|> try incrementing
       <|> try device

register_literal = findReserved registerNames

register :: Parser Register
register = registerFromString <$> register_literal
    where
        registerFromString ('R':xs) = Register (read xs :: Int)

direct :: Parser Operand
direct = Direct <$> register

immediate :: Parser Operand
immediate = Immediate <$ char '#' <*> expression

absolute :: Parser Operand
absolute = Absolute <$> expression <* notFollowedBy (symbol "(")

indirect :: Parser Operand
indirect = Indirect <$> parens register <* notFollowedBy (symbol "+")

-- NOTE: indirectOffset doesn't accept full blown expression
indirectOffset :: Parser Operand
indirectOffset = IndirectOffset <$> (exprInteger <|> variable)
                                <*> parens register

-- NOTE: relativeOffset doesn't accept full blown expression
relativeOffset :: Parser Operand
relativeOffset = RelativeOffset <$> (exprInteger <|> variable)
                                <* parens (reserved "PC")

decrementing :: Parser Operand
decrementing = Decrementing <$ reservedOp "-" <*> parens register

incrementing :: Parser Operand
incrementing = Incrementing <$> parens register <* reservedOp "+"

device :: Parser Operand
device = unexpected "Hit device matching"

instructionBody :: Parser InstructionBody
instructionBody = try mnemonic0 <|> try mnemonic1 <|> try mnemonic2

instruction :: Parser AsmStmt
instruction = Instruction <$> asmLabel <*> instructionBody

asmStmt :: Parser AsmStmt
asmStmt = instruction <|> assignment

stmtList :: Parser [AsmStmt]
stmtList = manyTill asmStmt eof

parseProgram = parse (whiteSpace >> stmtList) ""
