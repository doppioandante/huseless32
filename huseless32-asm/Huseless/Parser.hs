module Huseless.Parser where

import Control.Monad (void)
import qualified Text.Megaparsec.Expr as Expr
import Text.Megaparsec
import Text.Megaparsec.String

import qualified Huseless.Lexer as Lex
import Huseless.Syntax

opEqual = Lex.lexeme Lex.opEqual
comma = Lex.lexeme Lex.comma
colon = Lex.lexeme Lex.colon
identifier = Lex.lexeme Lex.identifier
signedLiteral = Lex.signedLiteral <* Lex.spaceConsumer
stringLiteral = Lex.stringLiteral <* Lex.spaceConsumer

binary name f = Expr.InfixL (Lex.symbol name >> return (BinaryOp f))

table = [[binary "*" Multiply]
        ,[binary "+" Plus,
          binary "-" Minus]]

expression :: Parser Expression
expression = Expr.makeExprParser term table <?> "expression"

term :: Parser Expression
term = exprInteger <|> variable <|> Lex.parens expression

variable ::  Parser Expression
variable = Variable <$> identifier

exprInteger :: Parser Expression
exprInteger = ExprValue <$> signedLiteral

assignment :: Parser AsmStmt
assignment = Assignment <$> identifier <* opEqual <*> expression

asmLabels :: Parser LabelList
asmLabels = LabelList <$> many (try identifier <* colon)

 -- TODO: is this efficient? Maybe lazyness is saving us
 -- (edit: maybe sorting should save us)
findReserved table =
    foldr1 (<|>) $
      (flip map) table $ \instr ->
        try $ Lex.symbol instr

mnemonic0 :: Parser InstructionBody
mnemonic0 = do
    name <- findReserved Lex.mnemonic0Names
    -- TODO: check validity early
    return $ Mnemonic0 name

mnemonic1 :: Parser InstructionBody
mnemonic1 = do
    name <- findReserved Lex.mnemonic1Names
    op <- operand
    -- TODO: check validity early
    return $ Mnemonic1 name op

mnemonic2 :: Parser InstructionBody
mnemonic2 = do
    name <- findReserved Lex.mnemonic2Names
    op1 <- operand
    Lex.comma
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

register_literal = findReserved Lex.registerNames

register :: Parser Register
register = registerFromString <$> register_literal <?> "register"
    where
        registerFromString ('R':xs) = Register (read xs :: Int)

direct :: Parser Operand
direct = Direct <$> register

immediate :: Parser Operand
immediate = Immediate <$ char '#' <*> expression <?> "immediate"

absolute :: Parser Operand
absolute = Absolute <$> expression <* notFollowedBy (char '(')

indirect :: Parser Operand
indirect = Indirect <$> Lex.parens register <* notFollowedBy (char '+')

-- NOTE: indirectOffset doesn't accept full blown expression
indirectOffset :: Parser Operand
indirectOffset = IndirectOffset <$> (exprInteger <|> variable)
                                <*> Lex.parens register

-- NOTE: relativeOffset doesn't accept full blown expression
relativeOffset :: Parser Operand
relativeOffset = RelativeOffset <$> (exprInteger <|> variable)
                                <* Lex.parens (string "PC")

decrementing :: Parser Operand
decrementing = Decrementing <$ char '-' <*> Lex.parens register

incrementing :: Parser Operand
incrementing = Incrementing <$> Lex.parens register <* char '+'

device :: Parser Operand
device = undefined

instructionBody :: Parser InstructionBody
instructionBody = try mnemonic0 <|> try mnemonic1 <|> try mnemonic2

instruction :: Parser AsmStmt
instruction = Instruction <$> asmLabels <*> instructionBody

globalDecl :: Parser AsmStmt
globalDecl = GlobalDecl <$ char '.' <* string "GLB"
                        <*> identifier `sepBy1` comma

externDecl :: Parser AsmStmt
externDecl = ExternDecl <$ char '.' <* string "EXT"
                        <*> identifier `sepBy1` comma

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
initList = InitList <$> initValue `sepBy1` comma

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

comment = Lex.symbol ";" >> anyChar `manyTill` (lookAhead eol)
newlineBreak = (void comment <|> Lex.spaceConsumer) *> eol

stmtList :: Parser [(Int, AsmStmt)]
stmtList = many (asmStmtWithLine <* skipSome newlineBreak)
  where
    asmStmtWithLine = do
      pos <- getPosition
      stmt <- asmStmt
      posAfter <- getPosition
      return (max (sourceLine pos) (sourceLine posAfter), stmt)

parseProgram = parse (try (many newlineBreak) >> stmtList) ""
-- parseProgram text = parse (space >> stmtList) "" text ++ "\n"
