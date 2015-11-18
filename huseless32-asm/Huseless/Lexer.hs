module Huseless.Lexer where

import Control.Applicative
import Control.Monad (void)
import Data.Char (digitToInt, isSpace, isLetter, isAlphaNum)
import Data.List (foldl')
import Text.Megaparsec.Combinator (between)
import qualified Text.Megaparsec.Lexer as L (hexadecimal, skipLineComment, skipBlockComment, space, decimal, signed, symbol, lexeme, charLiteral)
import Text.Megaparsec.Prim (Stream, ParsecT)
import Text.Megaparsec (oneOf, char, (<?>), skipSome, satisfy, unexpected, manyTill, skipMany)
import Text.Megaparsec.String

mnemonic0Names = [
    "HALT",
    "NOP",
    "RESET",
    "TRACE",
    "PUSHSR",
    "POPSR",
    "CLCR",
    "CLRN",
    "CLRZ",
    "CLRV",
    "CLRP",
    "CLRI",
    "SETC",
    "SETN",
    "SETZ",
    "SETV",
    "SETP",
    "SETI",
    "RET",
    "RTI"
    --, "ILLEGAL"
    ]

mnemonic1Names = [
    "TRAP",
    "PUSH",
    "POP",
    "MOVFRSR",
    "MOVTOSR",
    "JMP",
    "JSR",
    "JC",
    "JN",
    "JZ",
    "JV",
    "JP",
    "JI",
    "JNC",
    "JNN",
    "JNZ",
    "JNV",
    "JNP",
    "JNI",
    "START",
    "CLEAR",
    "SETIM",
    "CLRIM"
    ]

mnemonic2Names = [
    "SMUL",
    "UMUL",
    "SDIV",
    "UDIV",
    "DSMUL",
    "DUMUL",
    "DSDIV",
    "DUDIV",
    "JR",
    "JNR",
    "JIM",
    "JNIM"
    ]
    ++ (sm "MOV")
    ++ (sm "MVL")
    ++ (sm "EXG")
    ++ (sm "ADD")
    ++ (sm "ADC")
    ++ (sm "CMP")
    ++ (sm "NEG")
    ++ (sm "SUB")
    ++ (sm "SBB")
    ++ (sm "AND")
    ++ (sm "OR")
    ++ (sm "XOR")
    ++ (sm "NOT")
    ++ (sm "ASL")
    ++ (sm "ASR")
    ++ (sm "LSL")
    ++ (sm "LSR")
    ++ (sm "RCL")
    ++ (sm "RCR")
    ++ (sm "ROL")
    ++ (sm "ROR")
    ++ (sm "IN")
    ++ (sm "OUT")
 where
     sm name = [name ++ z | z <- ["B", "W", "L"]]

registerNames = ['R':[idx] | idx <- ['0' .. '7']]

mnemonicNames = mnemonic0Names ++ mnemonic1Names ++ mnemonic2Names
reservedNames = mnemonicNames ++ registerNames ++ ["PC", "GLB", "EXT"]
--        style = emptyDef {
--            Tok.reservedNames = registerNames ++ mnemonicNames ++ ["PC", "GLB", "EXT"],
--            Tok.reservedOpNames = ["+", "-", "*", "="],
--            Tok.opLetter = oneOf "+-*",
--            Tok.tokIsSpace = \c -> isSpace c && c /= '\n'
--            }
simpleSpace :: Stream s m Char => ParsecT s u m Char
simpleSpace = satisfy (\c -> isSpace c && c /= '\n' && c /= '\r')

spaceConsumer :: Stream s m Char => ParsecT s u m ()
spaceConsumer = skipMany simpleSpace

nonBreakingSpace :: Stream s m Char => ParsecT s u m ()
nonBreakingSpace = skipSome simpleSpace <?> "non-breaking space"

parens :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
parens = between (symbol "(") (symbol ")")

symbol :: Stream s m Char => String -> ParsecT s u m String
symbol = L.symbol spaceConsumer
lexeme :: Stream s m Char => ParsecT s u m String -> ParsecT s u m String
lexeme = L.lexeme spaceConsumer

hexLiteral :: Stream s m Char => ParsecT s u m Integer
hexLiteral = char '$' >> L.hexadecimal <?> "hexadecimal literal"

signedLiteral :: Stream s m Char => ParsecT s u m Integer
signedLiteral = L.signed spaceConsumer (hexLiteral <|> L.decimal) <?> "integer literal"

opEqual :: Stream s m Char => ParsecT s u m String
opEqual = symbol "="

comma :: Stream s m Char => ParsecT s u m String
comma = symbol ","

colon :: Stream s m Char => ParsecT s u m String
colon = symbol ":"
-- A valid identifier cannot start with a digit
-- Following alphanumeric characters and '_' are valid
-- No valid identifier can clash with opcode mnemonics or register names
identifier :: Stream s m Char => ParsecT s u m String
identifier = do
    idHead <- satisfy (\c -> isLetter c || c == '_')
    idTail <- many $ satisfy (\c -> isAlphaNum c || c == '_')
    let id = idHead : idTail
     in if isReserved id
           then unexpected $ id ++ " is a reserved name"
           else return id
    where
        -- TODO: sort
        isReserved name = any (== name) reservedNames


stringLiteral :: Stream s m Char => ParsecT s u m String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

