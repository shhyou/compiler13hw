module Main (main) where

import System.IO (readFile)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))

import qualified Language.BLang.FrontEnd.Parser as Parser
--import qualified Language.BLang.Debug.ParserAST as D

import Language.BLang.Homework.Homework4

import Control.Monad (mapM_)
import Control.Monad.Trans (liftIO)

import Control.Monad.Writer
import Language.BLang.Semantic.ConstExprFolding
import Language.BLang.Semantic.DesugarAST
import Language.BLang.Semantic.SymTable
import Language.BLang.Semantic.TypeCheck

printParseTree indent (Parser.Terminal a) =
  putStrLn $ indent ++ "Terminal " ++ (show $ fmap (const "") a)
printParseTree indent (Parser.NonTerminal xs) = do
  putStrLn $ indent ++ "NonTerminal"
  mapM_ (printParseTree ("  " ++ indent)) xs

test str = do
  let Right (tree, ast) = Parser.parse str
  liftIO $ printParseTree "" tree
  (prog, ces) <- runWriterT $ do
    foldedAST <- constFolding ast
    liftIO $ putStrLn "folded"
    noTCustomAST <- tyDesugar foldedAST
    liftIO $ putStrLn "tyDesugar"
    let arrptrAST = fnArrDesugar noTCustomAST
    symAST <- buildSymTable arrptrAST
    liftIO $ putStrLn "buildSymTable"
    typeCheck symAST
  mapM_ (putStrLn . show) ces
  return ces

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
  print ast
  printParseTree "" parseTree
  let (prog, ces) = semanticCheck ast
  mapM_ (putStrLn . show) ces
