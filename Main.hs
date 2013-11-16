{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import qualified Language.BLang.FrontEnd.Lexer as Lexer
import qualified Language.BLang.FrontEnd.Parser as Parser

import qualified Language.BLang.FrontEnd.ParseMonad as ParseMonad -- for testing
import Data.Foldable (foldlM)
import Data.List (intersperse)
import Control.Monad (mapM_)
import Control.Monad.Reader

lexStream :: String -> Either ParseMonad.ParseError [Lexer.Token]
lexStream = ParseMonad.runParser lexStream'
  where lexStream' :: ParseMonad.Parser [Lexer.Token]
        lexStream' = Lexer.lexer $ \token ->
                      case token of
                        Lexer.EOF -> return [Lexer.EOF]
                        _         -> return . (token:) =<< lexStream'

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
showExpr (Parser.Expr op2 [lhs,rhs]) = wrapParen $ showExpr lhs
                                     . (' ':). ((showOp op2) ++) . (' ':)
                                     . showExpr rhs
showExpr (Parser.Expr op1 [expr]) = wrapParen $ ((showOp op1) ++) . (showExpr expr)
showExpr (Parser.Ap callee args) = showExpr callee . ('(':)
                                 . foldr (.) id (intersperse (',':) (map showExpr args))
                                 . (')':)
showExpr (Parser.Identifier s) = (s ++)
showExpr (Parser.LiteralVal (Parser.IntLiteral n)) = shows n
showExpr (Parser.LiteralVal (Parser.FloatLiteral f)) = shows f . ('f':)
showExpr (Parser.LiteralVal (Parser.StringLiteral s)) = (s ++)
showExpr (Parser.ArrayRef ele idx) = showExpr ele . ('[':) . showExpr idx .(']':)

main :: IO ()
main = do
  input <- getContents
  case lexStream input of
    Left parseError -> putStrLn "[ERROR] [LEXER]" >> putStrLn (show parseError)
    Right stream    -> putStrLn "[TOKEN]" >> mapM_ (\a -> putStr "  " >> print a) stream
  case Parser.parse input of
    Left parseError -> putStrLn "[ERROR] [PARSER]" >> putStrLn (show parseError)
    Right ast       -> putStrLn "[AST]" >> runReaderT (printCode ast) ""
