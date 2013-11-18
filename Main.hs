module Main (main) where

import System.IO (readFile)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))

import qualified Language.BLang.FrontEnd.Parser as Parser
import qualified Language.BLang.Homework.Homework3 as Homework3

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
  let Right ast = parseResult
  Homework3.printAST ast
