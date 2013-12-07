{-# LANGUAGE FlexibleContexts #-}

module Language.BLang.Debug.ParserAST (printAST) where

import Data.Foldable (foldlM)
import Data.List (intersperse)
import Control.Monad (mapM_)
import Control.Monad.Reader

import qualified Language.BLang.FrontEnd.Parser as Parser

printAST :: Parser.AST -> IO ()
printAST ast = runReaderT (printCode ast) ""

pStr0 :: MonadIO m => String -> m ()
pStr0 = liftIO . putStr

pStrLn0 :: MonadIO m => String -> m ()
pStrLn0 = liftIO . putStrLn

pStr :: (MonadReader String m, MonadIO m) => String -> m ()
pStr str = do
  pStr0 =<< ask
  pStr0 str

pStrLn :: (MonadReader String m, MonadIO m) => String -> m ()
pStrLn str = pStr str >> liftIO (putChar '\n')

wrapParen :: (String -> String) -> String -> String
wrapParen s = ('(':) . s . (')':)

showOp :: Parser.Operator -> String
showOp Parser.Plus = "+"
showOp Parser.Minus = "-"
showOp Parser.Times = "*"
showOp Parser.Divide = "/"
showOp Parser.Negate = "-"
showOp Parser.LT = "<"
showOp Parser.GT = ">"
showOp Parser.LEQ = "<="
showOp Parser.GEQ = ">="
showOp Parser.EQ = "=="
showOp Parser.NEQ = "/="
showOp Parser.LOr = "||"
showOp Parser.LAnd = "&&"
showOp Parser.LNot = "!"
showOp Parser.Assign = ":="

showType :: Parser.Type -> String -> String
showType Parser.TInt = ("Int" ++)
showType Parser.TFloat = ("Float" ++)
showType Parser.TVoid = ("Void" ++)
showType Parser.TChar = ("Char" ++)
showType (Parser.TPtr t) = ("Ref(" ++) . showType t . (')':)
showType (Parser.TArray dims t) = (showType t) . foldr (\d f -> ('[':) . d . (']':) . f) id (map showExpr dims)
showType (Parser.TCustom t) = (t ++)

printCode :: (MonadIO m, MonadReader String m) => Parser.AST -> m ()
printCode [] = return ()
printCode ((Parser.VarDeclList decls):xs) = printVarDecls decls >> printCode xs
printCode ((Parser.FuncDecl ret nam arg cod):xs) = printFuncDecl ret nam arg cod >> printCode xs
  where printFuncDecl ret nam arg cod = do
          pStr ("def " ++ nam ++ "(")
          foldlM (\sep (var,typ) -> do
                    pStr0 $ sep ++ var ++ ": " ++ showType typ ""
                    return ", ")
                 "" arg
          pStrLn0 ("): " ++ showType ret " =")
          printStmt cod

printVarDecls :: (MonadIO m, MonadReader String m) => [Parser.ASTDecl] -> m ()
printVarDecls [] = return ()
printVarDecls ((Parser.TypeDecl vs):vss) = do
  mapM_ (\(var, typ) -> pStrLn $ "type " ++ var ++ " = " ++ showType typ [] ++ ";") vs
  printVarDecls vss
printVarDecls ((Parser.VarDecl vs):vss) = do
  mapM_ (\(var, typ, init) -> pStrLn $ "var " ++ var ++ ": " ++ showType typ (showInit init)) vs
  printVarDecls vss
  where showInit Nothing = ";"
        showInit (Just val) = " = " ++ showExpr val ";"

printBlock :: (MonadIO m, MonadReader String m) => Parser.ASTStmt -> m ()
printBlock expr@(Parser.Block _ _) = printStmt expr
printBlock expr                    = local ("  " ++) (printStmt expr)

printStmt :: (MonadIO m, MonadReader String m) => Parser.ASTStmt -> m ()
printStmt (Parser.Block decls stmts) = do
  pStrLn "{"
  local ("  " ++) (printVarDecls decls)
  local ("  " ++) (mapM_ printStmt stmts)
  pStrLn "}"
printStmt (Parser.For init cond iter code) = do
  pStr "for ("
  foldlM (\sep stmt -> pStr0 (sep ++ showExpr stmt "") >> return ", ") "" init
  pStr0 "; "
  foldlM (\sep stmt -> pStr0 (sep ++ showExpr stmt "") >> return ", ") "" cond
  pStr0 "; "
  foldlM (\sep stmt -> pStr0 (sep ++ showExpr stmt "") >> return ", ") "" iter
  pStrLn0 ")"
  printBlock code
printStmt (Parser.While cond code) = do
  pStr "while ("
  foldlM (\sep stmt -> pStr0 (sep ++ showExpr stmt "") >> return ", ") "" cond
  pStrLn0 ")"
  printBlock code
printStmt (Parser.If con th el) = do
  pStrLn ("if " ++ wrapParen (showExpr con) " then")
  printBlock th
  case el of
    Just el -> pStrLn "else" >> printBlock el
    Nothing -> return ()
printStmt (Parser.Return Nothing) = do
  pStrLn "return();"
printStmt (Parser.Return (Just val)) = do
  pStrLn $ "return" ++ wrapParen (showExpr val) ";"
printStmt Parser.Nop = do
  pStrLn ";"
printStmt expr = pStrLn (showExpr expr ";")

showExpr :: Parser.ASTStmt -> String -> String
showExpr (Parser.Expr op2 [lhs,rhs]) = wrap $ showExpr lhs
                                     . (' ':). ((showOp op2) ++) . (' ':)
                                     . showExpr rhs
  where wrap = case op2 of { Parser.Assign -> id; _ -> wrapParen }
showExpr (Parser.Expr op1 [expr]) = wrapParen $ ((showOp op1) ++) . (showExpr expr)
showExpr (Parser.Ap callee args) = showExpr callee . ('(':)
                                 . foldr (.) id (intersperse (',':) (map showExpr args))
                                 . (')':)
showExpr (Parser.Identifier s) = (s ++)
showExpr (Parser.LiteralVal (Parser.IntLiteral n)) = shows n
showExpr (Parser.LiteralVal (Parser.FloatLiteral f)) = shows f . ('f':)
showExpr (Parser.LiteralVal (Parser.StringLiteral s)) = (s ++)
showExpr (Parser.ArrayRef ele idx) = showExpr ele . ('[':) . showExpr idx .(']':)
