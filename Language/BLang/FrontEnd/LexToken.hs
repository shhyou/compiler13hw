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
             deriving (Functor, Show)

data Literal = IntLiteral Integer
             | FloatLiteral Double
             | StringLiteral String
             deriving (Show)

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
