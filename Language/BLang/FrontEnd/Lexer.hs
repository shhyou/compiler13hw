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
           deriving (Show)

data Literal = IntLiteral Integer
             | FloatLiteral Double
             | StringLiteral String
             deriving (Show)

symbols = [(["+","-","*","/"], SymArithmetic),
           (["<=", ">=", "!=", "==", "<", ">"], SymRelational), -- note that the order is essential
           (["||", "&&", "!"], SymLogic),
           (["="], const SymAssign),
           (["{", "}", "[", "]", "(", ")", ";", ",", "."], SymSeparator)]

lexer :: (Token -> IO a) -> IO a
lexer = undefined
