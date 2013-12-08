{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.BLang.FrontEnd.ParseMonad (
  Parser,
  runParser,
  getInput,
  getCurrLine,
  advance,
  tokenStackSize,
  pushToken,
  popTokens
) where

import Control.Applicative -- also for the functor instances
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Error

import Language.BLang.Data
import Language.BLang.Error
import Language.BLang.FrontEnd.LexToken

newtype Parser a = Parser { unParser :: StateT ParseState (ErrorT CompileError Identity) a }
                  deriving (Monad, Functor, MonadState ParseState, MonadError CompileError)

data ParseState = ParseState { getParseInput :: String, currParseLine :: Line, getTokenStack :: [Token Line] }

runParser :: Parser a -> String -> Either CompileError a
runParser (Parser p) input =
  fmap fst $
  runIdentity $
  runErrorT $
  runStateT p (ParseState input (beginOfLine 1 input) [])

getInput :: Parser String
getInput = getParseInput <$> get

getCurrLine :: Parser Line
getCurrLine = currParseLine <$> get

infixl 1 .->
(.->) = flip ($)

setCol col line = line { colNo = col }
setLineNo lineno line = line { lineNo = lineno }

setInput str st = st { getParseInput = str }
setCurrLine line st = st { currParseLine = line }
setTokenStack stk st = st { getTokenStack = stk }

advance :: Int -> Parser ()
advance n
  | n <= 0 = return ()
advance n = do
  state@(ParseState str (currLine@Line{ lineNo = line, colNo = col }) _) <- get
  let newLine xs = state.->setInput(xs).->setCurrLine(beginOfLine (line + 1) xs)
      nextCol = state.->setInput(tail str).->setCurrLine(currLine.->setCol(col + 1))
  case str of
    '\r':'\n':xs -> put (newLine xs) >> advance (n - 2)
    x:xs -> do (if x `elem` "\r\n" then put (newLine xs) else put nextCol)
               advance (n - 1)
    _ -> return ()

tokenStackSize :: Parser Int
tokenStackSize = length . getTokenStack <$> get

pushToken :: Token Line -> Parser ()
pushToken tok = modify (getTokenStack >>= (.->setTokenStack) . (tok:))

popTokens :: Int -> Parser [Token Line]
popTokens n = do
  state <- get
  let (popped, rest) = splitAt n (getTokenStack state)
  put $ state.->setTokenStack(rest)
  return popped
