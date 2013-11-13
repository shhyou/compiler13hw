module Language.BLang.FrontEnd.Lexer (
  Token(..),
  Literal(..),
  lexer
) where

--import ParserMonad (Parser)

data Token = LiteralToken Literal
           | ID String
           | SymArithmetic String
           | SymRelational String
           | SymLogic String
           | SymAssign
           | SymSeparator String
           | EOF

data Literal = IntLiteral Integer
             | FloatLiteral Double
             | StringLiteral String

symbols = [(["+","-","*","/"], SymArithmetic),
           (["<=", ">=", "!=", "==", "<", ">"], SymRelational), -- note that the order is essential
           (["||", "&&", "!"], SymLogic),
           (["="], const SymAssign),
           (["{", "}", "[", "]", "(", ")", ";", ",", "."], SymSeparator)]

lexer :: (Token -> IO a) -> IO a
lexer = undefined
