module Main (main) where

import qualified Language.BLang.FrontEnd.Lexer as Lexer
import qualified Language.BLang.FrontEnd.Parser as Parser

import qualified Language.BLang.Homework.Homework3 as Homework3

import qualified Language.BLang.FrontEnd.ParseMonad as ParseMonad -- for testing
import qualified Language.BLang.Debug.ParserAST as Debug.ParserAST (printAST)

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
{-
  case lexStream input of
    Left parseError -> putStrLn "[ERROR] [LEXER]" >> putStrLn (show parseError)
    Right stream    -> putStrLn "[TOKEN]" >> mapM_ (\a -> putStr "  " >> print a) stream
-}
  case Parser.parse input of
    Left parseError -> putStrLn "[ERROR] [PARSER]" >> putStrLn (show parseError)
    Right ast       -> Homework3.printAST ast
