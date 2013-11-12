module Main (main) where

import FrontEnd.Lexer
import qualified FrontEnd.Parser as Parser

main :: IO ()
main = do
  Parser.parse [ID "return"]
  Parser.parse [ID "main"]
  Parser.parse [SymLogic "&&"]
