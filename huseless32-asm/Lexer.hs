module Lexer where

import Data.Char (digitToInt)
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
    where
        --names = [],
        style = emptyDef {
            Tok.commentLine = ";",
            Tok.reservedOpNames = ["+", "-", "*"],
            Tok.opLetter = oneOf "+-*"
            }

-- RIPOFF, thanks Parsec.Token <3
bnumber base baseDigit
        = do{ digits <- many1 baseDigit
            ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
            ; seq n (return n)
            }

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
