module MIPSTest where

import Data.List
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Control.Monad
import Control.Monad.Writer
import Control.Applicative hiding (Const)

import Language.BLang.Data
import Language.BLang.Error

import qualified Language.BLang.FrontEnd.Parser as P
import Language.BLang.Semantic.ConstExprFolding
import Language.BLang.Semantic.DesugarType
import Language.BLang.Semantic.SymTable
import Language.BLang.Semantic.TypeCheck
import Language.BLang.Semantic.NormalizeAST (normalize)
import qualified Language.BLang.CodeGen.LLIRTrans as LLIRTrans
import qualified Language.BLang.CodeGen.MIPSTrans as MIPSTrans

import qualified Language.BLang.CodeGen.LLIR as L
import qualified Language.BLang.CodeGen.AsmIR as A
import Language.BLang.Semantic.Type

test' :: String -> IO String
test' str = do
  putStrLn str
  let Right parsedAST = P.parse str
  (prog, _) <- runWriterT $ do
    foldedAST <- constFolding parsedAST
    typeInlinedAST <- tyDesugar foldedAST
    let decayedAST = fnArrDesugar typeInlinedAST
    symbolAST <- buildSymTable decayedAST
    typedAST <- typeCheck symbolAST
    return $ normalize typedAST
  llir@(L.Prog llirGlobl llirFuncs llirRegs) <- LLIRTrans.llirTrans prog
  putStrLn $ "global: " ++ show (map snd $ toListA llirGlobl)
  putStrLn $ "regs: " ++ show (reverse $ toListA llirRegs)
  T.mapM print llirFuncs
  putStrLn "============================================================="
  mips <- MIPSTrans.transProg llir
  return (show mips)

writeToFile = ""
--writeToFile = "test.s"

test :: String -> IO ()
test = if null writeToFile
        then putStrLn <=< test'
        else writeFile writeToFile <=< test'

test1 = test "int main() { int a[10], i; for (i = 0; i < 10; i=i+1) a[i] = i * i; return 0; }"
test2 = test "int main() { int a = 2, b = 3 + 5; return (a+b) - (a-b); }"
test3 = test "int main() { float a = 2.0, b = 3.0 + 5.0; return (a+b) - (a-b); }"
test4 = test "int main() { float a = 3.14; if (4.0 > a && a > 2.71) return 3; else return -2; }"
test5 = test "int main() { write(\"Hello World!\n\"); return 0; }"
test6 = test "int f() { return 0; } int main() { int a = 1, b = 8; return a + b + f(); }"
test7 = test "int main() { if (1.0 > 2.0) write(1.0); else write(2); if (1.0 < 2.0) write(1.0); else write(2); }" -- 2, 1.0
