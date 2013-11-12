module FrontEnd.Lexer (
  Token(..),
  Literal(..),
  lexer
) where

data Token = LiteralToken Literal
           | ID String
           | SymArithmetic String
           | SymRelational String
           | SymLogic String
           | SymAssign
           | SymSeparator String

data Literal = IntLiteral Integer
             | FloatLiteral Double
             | StringLiteral String

symbols = [(["+","-","*","/"], SymArithmetic),
           (["<=", ">=", "!=", "==", "<", ">"], SymRelational), -- note that the order is essential
           (["||", "&&", "!"], SymLogic),
           (["="], const SymAssign),
           (["{", "}", "[", "]", "(", ")", ";", ",", "."], SymSeparator)]

lexer :: String -> [Token]
lexer = undefined
