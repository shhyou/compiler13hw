{-# LANGUAGE DeriveFunctor #-}

module Language.BLang.FrontEnd.LexToken (
  Token(..),
  Literal(..),
  showToken,
  getTokenData,
  getTokenLen
) where

data Token a = LiteralToken Literal Integer a
             | Identifier String Integer a
             | SymArithmetic String Integer a
             | SymRelational String Integer a
             | SymLogic String Integer a
             | SymAssign Integer a
             | SymSeparator String Integer a
             | EOF Integer a
             deriving (Functor)

data Literal = IntLiteral Integer
             | FloatLiteral Double
             | StringLiteral String
             deriving (Show, Eq)

instance Show (Token a) where
  show (LiteralToken (IntLiteral n) _ _) = "integer literal " ++ show n
  show (LiteralToken (FloatLiteral f) _ _) = "float literal " ++ show f
  show (LiteralToken (StringLiteral s) _ _) = "string literal " ++ show s
  show (Identifier s _ _) = "identifier '" ++ s ++ "'"
  show (SymArithmetic s _ _) = "symbol '" ++ s ++ "'"
  show (SymRelational s _ _) = "symbol '" ++ s ++ "'"
  show (SymLogic s _ _) = "symbol '" ++ s ++ "'"
  show (SymAssign _ _) = "symbol '='"
  show (SymSeparator s _ _) = "symbol '" ++ s ++ "'"
  show (EOF _ _) = "end of file"

showToken :: Token a -> String
showToken = show . fmap (const ())

getTokenData :: Token a -> a
getTokenData (LiteralToken _ _ a) = a
getTokenData (Identifier _ _ a) = a
getTokenData (SymArithmetic _ _ a) = a
getTokenData (SymRelational _ _ a) = a
getTokenData (SymLogic _ _ a) = a
getTokenData (SymAssign _ a) = a
getTokenData (SymSeparator _ _ a) = a
getTokenData (EOF _ a) = a

getTokenLen :: Token a -> Integer
getTokenLen (LiteralToken _ len _) = len
getTokenLen (Identifier _ len _) = len
getTokenLen (SymArithmetic _ len _) = len
getTokenLen (SymRelational _ len _) = len
getTokenLen (SymLogic _ len _) = len
getTokenLen (SymAssign len _) = len
getTokenLen (SymSeparator _ len _) = len
getTokenLen (EOF len _) = len
