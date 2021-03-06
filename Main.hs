module Main (main) where

import Control.Monad
import Control.Monad.Writer
import Data.List (sortBy)
import qualified Data.Traversable as T (mapM)
import System.IO (readFile, writeFile)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))

import Language.BLang.Data
import Language.BLang.Error

import qualified Language.BLang.FrontEnd.Parser as Parser
import qualified Language.BLang.Semantic.ConstExprFolding as Const
import qualified Language.BLang.Semantic.DesugarType as Desugar
import qualified Language.BLang.Semantic.SymTable as SymTable
import qualified Language.BLang.Semantic.TypeCheck as TypeCheck
import qualified Language.BLang.Semantic.NormalizeAST as NormalizeAST
import qualified Language.BLang.BackEnd.SethiUllman as SethiUllman
import qualified Language.BLang.BackEnd.LLIRTrans as LLIRTrans
import qualified Language.BLang.BackEnd.EmptyBlockElim as EmptyBlockElim
import qualified Language.BLang.CodeGen.MIPSTrans as MIPSTrans
import qualified Language.BLang.CodeGen.BlockOrder as BlockOrder

import qualified Language.BLang.BackEnd.LLIR as LLIR

exit1 = exitWith (ExitFailure 1)

main :: IO ()
main = do
  args <- getArgs
  let (inputStream, outputStream) = case args of
        [] -> (getContents, putStrLn)
        [file] -> (readFile file, writeFile "output.s")
        [infile, outfile] -> (readFile infile, writeFile outfile)
        _ -> (putStrLn "Error: incorrect command line args" >> exit1, const (return ()))

  {- read input & parsing -}
  putStrLn "[+] BLang: parsing..."
  input <- inputStream
  let parseResult = Parser.parse input
  case parseResult of
    Left parseError -> putStrLn (show parseError) >> exit1
    _ -> return ()
  let Right parsedAST = parseResult

  {- semantic check -}
  putStrLn "[+] BLang: semantic check..."
  let compareCompileError ce1 ce2 = compare (errLine ce1) (errLine ce2)
  (ast, ces) <- runWriterT $ censor (sortBy compareCompileError) $ do
    foldedAST <- Const.constFolding parsedAST
    typeInlinedAST <- Desugar.tyDesugar foldedAST
    let decayedAST = Desugar.fnArrDesugar typeInlinedAST
    symbolAST <- SymTable.buildSymTable decayedAST
    typedAST <- TypeCheck.typeCheck symbolAST
    return $ NormalizeAST.normalize typedAST
  when (not $ null ces) $ mapM_ (putStrLn . show) ces >> exit1

  {- code generation -}
  let adjustedAST = SethiUllman.seull ast
      llir = LLIRTrans.llirTrans adjustedAST
      inlinedLLIR = EmptyBlockElim.elim llir
      llirFuncs = LLIR.progFuncs inlinedLLIR
      llirGlobl = LLIR.progVars inlinedLLIR
      llirRegs  = LLIR.progRegs inlinedLLIR
  putStrLn "[+] BLang: code generation (I)..."
  putStrLn $ "global: " ++ show (map snd $ toListA llirGlobl)
  putStrLn $ "regs: " ++ show (reverse $ toListA llirRegs)
  T.mapM print llirFuncs
  let mips = MIPSTrans.transProg $ inlinedLLIR
      simpMips = BlockOrder.jumpElim . BlockOrder.blockOrder $ mips
  putStrLn "[+] BLang: code generation (II)..."
  outputStream $ show simpMips
  putStrLn "[+] BLang: done"
