module Main (main) where

import Control.Monad
import Control.Monad.Writer
import Data.List (sortBy)
import System.IO (readFile)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))

import Language.BLang.Error

import qualified Language.BLang.FrontEnd.Parser as Parser
import qualified Language.BLang.Semantic.ConstExprFolding as Const
import qualified Language.BLang.Semantic.DesugarType as Desugar
import qualified Language.BLang.Semantic.SymTable as SymTable
import qualified Language.BLang.Semantic.TypeCheck as TypeCheck
import qualified Language.BLang.Semantic.NormalizeAST as NormalizeAST

exit1 = exitWith (ExitFailure 1)

main :: IO ()
main = do
  args <- getArgs
  input <- case args of
            [] -> getContents
            [file] -> readFile file
            _ -> putStrLn "Error: incorrect command line args" >> exit1

  let parseResult = Parser.parse input
  case parseResult of
    Left parseError -> putStrLn (show parseError) >> exit1
    _ -> return ()
  let Right parsedAST = parseResult

  let compareCompileError ce1 ce2 = compare (errLine ce1) (errLine ce2)
  (prog, ces) <- runWriterT $ censor (sortBy compareCompileError) $ do
    foldedAST <- Const.constFolding parsedAST
    typeInlinedAST <- Desugar.tyDesugar foldedAST
    let decayedAST = Desugar.fnArrDesugar typeInlinedAST
    symbolAST <- SymTable.buildSymTable decayedAST
    TypeCheck.typeCheck symbolAST
  when (not $ null ces) $ mapM_ (putStrLn . show) ces >> exit1
  print prog
