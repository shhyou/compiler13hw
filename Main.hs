module Main (main) where

import qualified Language.BLang.FrontEnd.Lexer as Lexer
import qualified Language.BLang.FrontEnd.Parser as Parser

main :: IO ()
main = do
  Parser.parse
