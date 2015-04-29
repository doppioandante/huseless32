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
variable = identifier >>= return . Variable

exprInteger :: Parser Expression
exprInteger = integer >>= return . ExprValue

factor :: Parser Expression
factor = exprInteger <|> variable <|> parens expression
