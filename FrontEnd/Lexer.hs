module FrontEnd.Lexer (
  Token(..),
  Literal(..),
  lexer
) where

data Token = LiteralToken Literal
           | ID String
           | SymArithmetic Char
           | SymRelational Char
           | SymLogic Char
           | SymAssign Char {- though this character can only be '=' -}
           | SymSeparator Char

data Literal = IntLiteral Integer
             | FloatLiteral Double
             | StringLiteral String

symbols = [(["+","-","*","/"], SymArithmetic),
           (["<=", ">=", "!=", "==", "<", ">"], SymRelational), -- note that the 
           (["||", "&&", "!"], SymLogic),
           (["="], SymAssign),
           (["{", "}", "[", "]", "(", ")", ";", ",", "."], SymSeparator)]

lexer = undefined
