module Main (main) where

import Data.Maybe (fromMaybe)
import Data.Foldable (foldlM)
import Control.Monad (liftM)
import System.Exit

import qualified Language.BLang.FrontEnd.Lexer as Lexer
import qualified Language.BLang.FrontEnd.Parser as Parser

import qualified Language.BLang.FrontEnd.ParseMonad as ParseMonad -- for testing
import qualified Language.BLang.Debug.ParserAST as Debug.ParserAST (printAST)

import qualified Language.BLang.Homework.Homework3 as Homework3

lexStream :: String -> Either ParseMonad.ParseError [Lexer.Token]
lexStream = ParseMonad.runParser lexStream'
  where lexStream' :: ParseMonad.Parser [Lexer.Token]
        lexStream' = Lexer.lexer $ \token ->
                      case token of
                        Lexer.EOF -> return [Lexer.EOF]
                        _         -> return . (token:) =<< lexStream'

main :: IO ()
main = do
  input <- getContents
  let tokens = lexStream input
      parsed = Parser.parse input
  case tokens of
    Left parseError -> do
      putStrLn "[ERROR] [LEXER]"
      putStrLn (show parseError)
      exitWith (ExitFailure 1)
    Right stream    -> return ()
  ast <- case parsed of
    Left parseError -> do
      putStrLn "[ERROR] [PARSER]"
      putStrLn (show parseError)
      exitWith (ExitFailure 2)
    Right ast       -> return ast
  Homework3.printAST (Homework3.fromAST ast)
