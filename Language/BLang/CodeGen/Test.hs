module Test where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.State

import Language.BLang.Data

import qualified Language.BLang.Semantic.AST as S
import qualified Language.BLang.CodeGen.LLIR as L

import qualified Language.BLang.FrontEnd.Parser as Parser
import qualified Language.BLang.Semantic.ConstExprFolding as Const
import qualified Language.BLang.Semantic.DesugarType as Desugar
import qualified Language.BLang.Semantic.SymTable as SymTable
import qualified Language.BLang.Semantic.TypeCheck as TypeCheck
import Language.BLang.CodeGen.LLIRTrans

newAST :: String -> S.Prog S.Var
newAST str =
  let Right parsedAST = Parser.parse str in
  let (prog, []) = runIdentity $ runWriterT $ do
        foldedAST <- Const.constFolding parsedAST
        typeInlinedAST <- Desugar.tyDesugar foldedAST
        let decayedAST = Desugar.fnArrDesugar typeInlinedAST
        symbolAST <- SymTable.buildSymTable decayedAST
        TypeCheck.typeCheck symbolAST
  in prog

fun :: String -> S.Prog S.Var -> S.AST S.Var
fun fn prog = S.funcCode (S.progFuncs prog ! fn)

printBlock :: Assoc L.Label [L.AST] -> IO ()
printBlock (Assoc ls) = forM_ ls $ \(lbl, codes) -> do
  print lbl
  forM_ codes $ \c -> putStrLn ("  " ++ show c ++ ";")

-- test expression, where `main` function should contain only one statemnet, which ought to be `return value`
testExpr :: String -> Assoc L.Label [L.AST]
testExpr str =
  let S.Block _ [S.Return _ (Just expr)] = fun "main" (newAST str) in
  let (prog, St nxtReg nxtBlk codes) =
        runIdentity $
        flip runStateT (St 0 0 emptyA) $ 
        cpsExpr expr (\val -> return [L.Return (Just val)]) in
  insertA (L.BlockLabel nxtBlk) prog codes

teste' = printBlock . testExpr

teste1 = teste' "int f(int a,int b){} int main() { return 1+f(1,2)*(3-4); }"
teste2 = teste' "float f(float b){} int main() { return 1+f(2)*3; }"
teste3 = teste' "int main() { int n,a[1][2][3]; return a[0][n][2-2]+5.0; }"
teste4 = teste' "int main() { int a, b; return (a+1 && b) + 8; }"
