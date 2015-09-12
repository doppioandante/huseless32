module Huseless.Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Expr

import Huseless.Lexer
import Huseless.Syntax

binary s f = Expr.Infix (reservedOp s >> return (BinaryOp f)) Expr.AssocLeft

table = [[binary "*" Multiply]
        ,[binary "+" Plus,
          binary "-" Minus]]

validNewlineBreak = many1 (optional whiteSpace <* newline <* optional whiteSpace)

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

asmLabels :: Parser LabelList
asmLabels = LabelList <$> many (identifier <* colon <* optional spaces)

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
    comma
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
       -- <|> try device

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
indirect = Indirect <$> parens register <* notFollowedBy (reservedOp "+")

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
device = undefined

instructionBody :: Parser InstructionBody
instructionBody = try mnemonic0 <|> try mnemonic1 <|> try mnemonic2

instruction :: Parser AsmStmt
instruction = Instruction <$> asmLabels <*> instructionBody

globalDecl :: Parser AsmStmt
globalDecl = GlobalDecl <$ char '.' <* reserved "GLB"
                        <*> commaSep1 identifier

externDecl :: Parser AsmStmt
externDecl = ExternDecl <$ char '.' <* reserved "EXT"
                        <*> commaSep1 identifier

pseudoDeclareStmt :: Parser PseudoInstructionBody
pseudoDeclareStmt = do
    char '.'
    (_:_:z) <- findReserved ["DSB", "DSW", "DSL"]
    let size = case z of
                   "B" -> SizeByte
                   "W" -> SizeWord
                   "L" -> SizeLWord
                   _   -> error $ "Invalid size matched: " ++ z
    expr <- expression
    return $ Declare size expr

initValue :: Parser InitValue
initValue =  (InitASCII <$> stringLiteral)
         <|> (InitExpr <$> expression)

initList :: Parser InitList
initList = InitList <$> commaSep1 initValue

pseudoInitStmt :: Parser PseudoInstructionBody
pseudoInitStmt = do
    char '.'
    (_:_:z) <- findReserved ["DCB", "DCW", "DCL"]
    let size = case z of
                   "B" -> SizeByte
                   "W" -> SizeWord
                   "L" -> SizeLWord
                   _   -> error $ "Invalid size matched: " ++ z
    list <- initList
    return $ Initialize size list

pseudoInstructionBody :: Parser PseudoInstructionBody
pseudoInstructionBody =  try pseudoDeclareStmt
                     <|> try pseudoInitStmt

pseudoInstruction = PseudoInstruction <$> asmLabels <*> pseudoInstructionBody

asmStmt :: Parser AsmStmt
asmStmt = try instruction
       <|> assignment
       <|> try pseudoInstruction
       <|> try globalDecl
       <|> try externDecl

stmtList :: Parser [(Int, AsmStmt)]
stmtList = many (asmStmtWithLine <* validNewlineBreak)
  where
    asmStmtWithLine = do
      pos <- getPosition
      stmt <- asmStmt
      posAfter <- getPosition
      return (max (sourceLine pos) (sourceLine posAfter), stmt)

parseProgram = parse (spaces >> stmtList) ""
