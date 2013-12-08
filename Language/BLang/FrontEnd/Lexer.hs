{-# LANGUAGE BangPatterns #-}

module Language.BLang.FrontEnd.Lexer (
  module LexToken,
  lexer
) where

import Text.Regex.Posix ((=~))
import Control.Monad.Error
import Control.Monad.State
import Data.Maybe (isJust)
import Numeric (readDec, readFloat)

import Language.BLang.Data
import Language.BLang.Error
import Language.BLang.FrontEnd.LexToken as LexToken
import Language.BLang.FrontEnd.ParseMonad (Parser, getInput, getCurrLine, advance, tokenStackSize, pushToken, popTokens)

type RawToken a = Integer -> a -> Token a

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
  let invoke k token = pushToken token >> k token
  input <- getInput
  line <- getCurrLine
  case input of
    [] ->
      invoke k (EOF 0 line)
    '/':'*':xs -> do
      len <- comment input
      advance len
      lexer k
    c:xs | c `elem` " \f\t\v\r\n" ->
      advance 1 >> lexer k
    '"':xs -> do
      str <- litString input
      advance (length str)
      invoke k (LiteralToken (StringLiteral str) (toInteger $ length str) line)
    otherwise ->
      case filter isJust $ map (tryMatch input) regExs of
        (Just (len, tokenConstructor)):_ -> do
          advance len
          invoke k (tokenConstructor (toInteger len) line)
        otherwise -> lexError "Input string does not match any token"
