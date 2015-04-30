module Huseless.Lexer where

import Data.Char (digitToInt)
import Data.List (foldl')
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok


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

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
    where
        mnemonicNames = mnemonic0Names ++ mnemonic1Names ++ mnemonic2Names
        style = emptyDef {
            Tok.commentLine = ";",
            Tok.reservedNames = registerNames ++ mnemonicNames ++ ["PC", "GLB", "EXT"],
            Tok.reservedOpNames = ["+", "-", "*", "="],
            Tok.opLetter = oneOf "+-*"
            }

-- RIPOFF, thanks Parsec.Token <3
bnumber base baseDigit = do
    digits <- many1 baseDigit
    let n = foldl' (\x d -> base*x + toInteger (digitToInt d)) 0 digits
    return n

hex_literal = char '$' *> bnumber 16 hexDigit
dec_literal = bnumber 10 digit

read_sign = do
    p <- char '+' <|> char '-'
    if p == '+'
       then return id
       else return negate


unsigned_literal = hex_literal <|> dec_literal

integer :: Parser Integer
integer = Tok.lexeme lexer (
    ($) <$> option id read_sign <*> unsigned_literal <?> "integer"
    )

parens = Tok.parens lexer
identifier = Tok.identifier lexer
reservedOp = Tok.reservedOp lexer
reserved = Tok.reserved lexer
whiteSpace = Tok.whiteSpace lexer
colon = Tok.colon lexer
comma = Tok.comma lexer
commaSep1 = Tok.commaSep1 lexer
stringLiteral = Tok.stringLiteral lexer
