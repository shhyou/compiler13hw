module Main (main) where

import System.IO (readFile)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))

import qualified Language.BLang.FrontEnd.Parser as Parser
import qualified Language.BLang.Debug.ParserAST as D

import Control.Monad (mapM_)

printParseTree indent (Parser.Terminal a) =
  putStrLn $ indent ++ "Terminal " ++ (show $ fmap (const "") a)
printParseTree indent (Parser.NonTerminal xs) = do
  putStrLn $ indent ++ "NonTerminal"
  mapM_ (printParseTree ("  " ++ indent)) xs

main :: IO ()
main = do
  args <- getArgs
  input <- case args of
            [] -> getContents
            [file] -> readFile file
            _ -> putStrLn "[ERROR] Incorrect command line args" >> exitWith (ExitFailure 1)
  let parseResult = Parser.parse input
  case parseResult of
    Left parseError -> do
      putStrLn "[ERROR] [PARSER]"
      putStrLn (show parseError)
      exitWith (ExitFailure 1)
    _ -> return ()
  let Right (parseTree, ast) = parseResult
  D.printAST ast
  printParseTree [] parseTree
