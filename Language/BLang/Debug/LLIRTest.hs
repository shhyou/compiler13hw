module LLIRTest where

import Data.List (sortBy)
import qualified Data.Traversable as T
import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

import Language.BLang.Data

import qualified Language.BLang.Semantic.AST as S
import qualified Language.BLang.BackEnd.LLIR as L

import qualified Language.BLang.FrontEnd.Parser as Parser
import qualified Language.BLang.Semantic.ConstExprFolding as Const
import qualified Language.BLang.Semantic.DesugarType as Desugar
import qualified Language.BLang.Semantic.SymTable as SymTable
import qualified Language.BLang.Semantic.TypeCheck as TypeCheck
import qualified Language.BLang.Semantic.NormalizeAST as NormalizeAST
import qualified Language.BLang.BackEnd.SethiUllman as SethiUllman
import Language.BLang.BackEnd.LLIRTrans
import qualified Language.BLang.BackEnd.EmptyBlockElim as EmptyBlockElim

newAST :: String -> S.Prog S.Type
newAST str =
  let Right parsedAST = Parser.parse str in
  let (prog, []) = runIdentity $ runWriterT $ do
        foldedAST <- Const.constFolding parsedAST
        typeInlinedAST <- Desugar.tyDesugar foldedAST
        let decayedAST = Desugar.fnArrDesugar typeInlinedAST
        symbolAST <- SymTable.buildSymTable decayedAST
        typedAST <- TypeCheck.typeCheck symbolAST
        return $ NormalizeAST.normalize typedAST
  in SethiUllman.seull prog

testFunc :: String -> IO () --IO (Assoc String (L.Func L.VarInfo))
testFunc str = do
  let prog = newAST str
  let llirProg = EmptyBlockElim.elim $ llirTrans prog
      funcs = L.progFuncs llirProg
      globl = L.progVars llirProg
      regs  = L.progRegs llirProg
  putStrLn $ "global: " ++ show (map snd $ toListA globl)
  putStrLn $ "regs: " ++ show (reverse $ toListA regs)
  T.mapM print funcs
  return ()

testf1 = testFunc "int b[3]; int f(int a[][5]) { a[1][0] = 5; return a[0][1] + b[2]; } int main() { int a[3][2][5]; return f(a[2]); }"
testf2 = testFunc "int f(int a[]) { return a[0]; } int main() { int a[5]; a[1] = 2; return f(a); }"
testf3 = testFunc "int f(int a[]) { return a[0]; } int main() { int n = 1, a[5]; a[n] = 2; return f(a); }"

printBlock :: Assoc L.Label [L.AST] -> IO ()
printBlock ls = forM_ (sortBy ((. fst) . compare . fst) $ toListA ls) $ \(lbl, codes) -> do
  print lbl
  forM_ codes $ \c -> putStrLn ("  " ++ show c ++ ";")

-- test expression, where `main` function should contain only one statement, which ought to be `return value`
testExpr :: String -> IO (Assoc L.Label [L.AST])
testExpr str = do
  let S.FuncDecl _ args vars [S.Return (Just expr)] = S.progFuncs (newAST str) ! "main"
  let (lbl, St nxtReg nxtBlk regs _ nilBlk codes) =
        runIdentity $
        flip runReaderT (map fst args) $
        flip runStateT (St 0 0 emptyA vars (error "not in a block") emptyA) $
        runNewControl (cpsExpr expr $ KFn $ \val -> return [L.Return (Just val)])
  print expr
  print regs
  return codes

teste' str = do
  blks <- testExpr str
  putStrLn str
  printBlock blks

teste1 = teste' "int f(int a,int b){} int main() { return 1+f(1,2)*(3-4); }"
teste2 = teste' "float f(float b){} int main() { return 1+f(2)*3; }"
teste3 = teste' "int main() { int n,a[1][2][3]; return a[0][n][2-2]+5.0; }"
teste4 = teste' "int main() { int a, b; return (a+1 && b) + 8; }"
teste5 = teste' "int main() { int a, b; return a && b; }"
teste6 = teste' "int main() { int a, b, c; return (c || (a && b)); }"
teste7 = teste' "int main() { int a, b, c; return (c && (a || b)); }"
teste8 = teste' "int main() { int a, b, c; return ((a && b) || c); }"
teste9 = teste' "int main() { int a, b, c; return ((a || b) && c); }"
teste10 = teste' "int main() { int a, b, c, d; return (a && b && c && d); }"
teste11 = teste' "int main() { int a, b, c; return ((a+1 && b) || c) + 8; }"
