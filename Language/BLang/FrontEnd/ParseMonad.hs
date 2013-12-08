{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.BLang.FrontEnd.ParseMonad (
  Parser,
  runParser,
  getInput,
  getCurrLine,
  advance
) where

import Control.Applicative -- also for the functor instances
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Error

import Language.BLang.Data
import Language.BLang.Error

newtype Parser a = Parser { unParser :: StateT ParseState (ErrorT CompileError Identity) a }
                  deriving (Monad, Functor, MonadState ParseState, MonadError CompileError)

data ParseState = ParseState { getParseInput :: String, currParseLine :: Line }

runParser :: Parser a -> String -> Either CompileError a
runParser (Parser p) input =
  fmap fst $
  runIdentity $
  runErrorT $
  runStateT p (ParseState input (beginOfLine 1 input))

getInput :: Parser String
getInput = getParseInput <$> get

getCurrLine :: Parser Line
getCurrLine = currParseLine <$> get

advance :: Int -> Parser ()
advance n
  | n <= 0 = return ()
advance n = do
  ParseState str (currLine@Line{ lineNo = line, colNo = col }) <- get
  let newLine xs = ParseState xs (beginOfLine (line+1) xs)
      nextCol = ParseState (tail str) currLine{ colNo = col + 1 }
  case str of
    '\r':'\n':xs -> put (newLine xs) >> advance (n - 2)
    x:xs -> do (if x `elem` "\r\n" then put (newLine xs) else put nextCol)
               advance (n - 1)
    _ -> return ()
