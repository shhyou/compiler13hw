module Language.BLang.FrontEnd.ParseMonad (
  Parser,
  ParseError,
  runParser,
  getInput,
  advance
) where

import Control.Applicative -- for the functor instances

newtype Parser a = Parser { unParser :: ParseState -> Either ParseError (a, ParseState) }

type InputStream = String
type ErrorMessage = String

--                current input current line  lineno   col
type ParseState = (InputStream, (InputStream, Integer, Integer))
newtype ParseError = ParseError (ErrorMessage, (InputStream, Integer, Integer))

instance Show ParseError where
  show (ParseError (msg, (content, line, col))) =
    msg ++ "\n" ++
    "At line " ++ show line ++ ", column " ++ show col ++ ":\n" ++
    fst (span (/= '\n') content)

instance Monad Parser where
  return a = Parser $ \s -> Right (a, s)
  m >>= f = Parser $ \s -> case unParser m s of
                             Right (a, s') -> unParser (f a) s'
                             Left notHappy -> Left notHappy
  fail message = Parser $ \(_, state) ->
                            Left $ ParseError (message, state)


runParser :: Parser a -> InputStream -> Either ParseError a
runParser p input = fmap fst $ unParser p (input, (input, 1, 1))

getInput :: Parser InputStream
getInput = Parser $ \s -> Right (fst s, s)

advance :: Int -> Parser ()
advance step = Parser $ \s -> Right ((), iterate advance' s !! step)
  where advance' ('\r':'\n':xs, (_, lineno, _)) = (xs, (xs, lineno+1, 1))
        advance' (x:xs, (_, lineno, _))
          | x `elem` "\r\n"                     = (xs, (xs, lineno+1, 1))
        advance' (x:xs, (line, lineno, col))    = (xs, (line, lineno, col+1))
        advance' state                          = state   -- reach EOF
