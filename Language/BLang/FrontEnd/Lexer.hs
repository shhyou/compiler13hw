{-# LANGUAGE BangPatterns, DeriveFunctor #-}

module Language.BLang.FrontEnd.Lexer (
  Token(..),
  Literal(..),
  showToken,
  getTokenData,
  getTokenLen,
  lexer
) where

import Text.Regex.Posix ((=~))
import Control.Monad.Error
import Control.Monad.State
import Data.Maybe (isJust)
import Numeric (readDec, readFloat)

import Language.BLang.Data
import Language.BLang.Error
import Language.BLang.FrontEnd.ParseMonad (Parser, getInput, getCurrLine, advance)

data Token a = LiteralToken Literal Integer a
             | Identifier String Integer a
             | SymArithmetic String Integer a
             | SymRelational String Integer a
             | SymLogic String Integer a
             | SymAssign Integer a
             | SymSeparator String Integer a
             | EOF Integer a
             deriving (Functor, Show)

type RawToken a = Integer -> a -> Token a

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

lexError :: String -> Parser a
lexError msg = do
  line <- getCurrLine
  throwError $ errorAt line msg

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

regExs :: [(String, String -> RawToken a)]
regExs = [(litFloat, LiteralToken . FloatLiteral . fst . head . readFloat),
          (litInt, LiteralToken . IntLiteral . fst . head . readDec),
          (identifier, Identifier)]
       ++ symbols

tryMatch :: String -> (String, String -> RawToken a) -> Maybe (Int, RawToken a)
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
        litString' _       = lexError "Unterminated string"
litString _ = lexError "The input is not a string"

comment :: String -> Parser Int
comment ('/':'*':xs) = comment' 2 xs
  where comment' !n ('*':'/':xs) = return (n + 2)
        comment' !n (_:xs)       = comment' (n+1) xs
        comment' _  _            = lexError "Unterminated comment"
comment _ = lexError "The input is not a comment"

lexer :: (Token Line -> Parser a) -> Parser a
lexer k = do
  input <- getInput
  line <- getCurrLine
  case input of
    [] ->
      k (EOF 0 line)
    '/':'*':xs -> do
      len <- comment input
      advance len
      lexer k
    c:xs | c `elem` " \f\t\v\r\n" ->
      advance 1 >> lexer k
    '"':xs -> do
      str <- litString input
      advance (length str)
      k (LiteralToken (StringLiteral str) (toInteger $ length str) line)
    otherwise ->
      case filter isJust $ map (tryMatch input) regExs of
        (Just (len, tokenConstructor)):_ -> advance len >> k (tokenConstructor (toInteger len) line)
        otherwise -> lexError "Input string does not match any token"
