{-# LANGUAGE BangPatterns #-}

module Language.BLang.FrontEnd.Lexer (
  Token(..),
  Literal(..),
  lexer
) where

import Text.Regex.Posix ((=~))
import Data.Maybe (isJust)
import Numeric (readDec, readFloat)

import Language.BLang.FrontEnd.ParseMonad (Parser, getInput, advance)

data Token = LiteralToken Literal
           | Identifier String
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

digit = "[0-9]"
letter = "[A-Za-z]"

symbols = [("(\\+|\\-|\\*|/)", SymArithmetic),
           ("(<=|>=|!=|==|<|>)", SymRelational), -- note that the order is essential
           ("(\\|\\||&&|!)", SymLogic),
           ("=", const SymAssign),
           ("(\\{|\\}|\\[|\\]|\\(|\\)|;|,|\\.)", SymSeparator)]

litFloat = concat ["((", digit, "*\\.", digit, "+|", digit, "+\\.)",
                   "([eE][+-]?", digit, "+)?",
                   "|(", digit, "+[eE][+-]?", digit, "+))"]
litInt = digit ++ "+"
identifier = concat ["(", letter, ")", "(", letter, "|", digit, "|_)*"]

regExs :: [(String, String -> Token)]
regExs = [(litFloat, LiteralToken . FloatLiteral . fst . head . readFloat),
          (litInt, LiteralToken . IntLiteral . fst . head . readDec),
          (identifier, Identifier)]
       ++ symbols

tryMatch :: String -> (String, String -> Token) -> Maybe (Int, Token)
tryMatch haystack (needle, makeToken) =
  if null before
    then Just (length matched, makeToken matched)
    else Nothing
  where (before, matched, _, _) = haystack =~ needle :: (String, String, String, [String])

litString :: String -> Parser String
litString ('"':xs) = return . ('"':) =<< litString' xs
  where litString' ('\\':c:xs) = return . ('\\':) . (c:) =<< litString' xs
        litString' ('"':_) = return "\""
        litString' (x:xs)  = return . (x:) =<< litString' xs
        litString' _       = fail "Unterminated string"
litString _ = fail "The input is not a string"

comment :: String -> Parser Int
comment ('/':'*':xs) = comment' 2 xs
  where comment' !n ('*':'/':xs) = return (n + 2)
        comment' !n (_:xs)       = comment' (n+1) xs
        comment' _  _            = fail "Unterminated comment"
comment _ = fail "The input is not a comment"

lexer :: (Token -> Parser a) -> Parser a
lexer k = do
  input <- getInput
  case input of
    [] ->
      k EOF
    '/':'*':xs -> do
      len <- comment input
      advance len
      lexer k
    c:xs | c `elem` " \f\t\v\r\n" ->
      advance 1 >> lexer k
    '"':xs -> do
      str <- litString input
      advance (length str)
      k (LiteralToken (StringLiteral str))
    otherwise ->
      case filter isJust $ map (tryMatch input) regExs of
        Just (len, tok):_ -> advance len >> k tok
        [] -> fail "Input string does not match any token"
