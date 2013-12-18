module Language.BLang.FrontEnd.Parser (module AST, parse) where
import Language.BLang.FrontEnd.AST as AST
import Language.BLang.FrontEnd.ParserHappy as ParserHappy

import Language.BLang.Data
import Language.BLang.Error
import qualified Language.BLang.FrontEnd.AST as AST
import qualified Language.BLang.FrontEnd.Lexer as Lexer (Token(..), Literal(..), getTokenData, getTokenLen)
import Language.BLang.FrontEnd.ParseMonad (runParser, pushTree, popTrees)

parse :: String -> Either CompileError (AST.ParseTree, AST.AST)
parse = runParser (do
  ast <- parser
  [(AST.Terminal (Lexer.EOF _ _)), AST.NonTerminal trees] <- popTrees 2
  let ast' = zipWith addLineTop trees ast
  return (AST.NonTerminal trees, ast'))

addLineTop :: AST.ParseTree -> AST.ASTTop -> AST.ASTTop
addLineTop (AST.NonTerminal ls) (AST.VarDeclList decls) =
  VarDeclList $ zipWith addLineDecl ls decls
addLineTop (AST.NonTerminal [AST.Terminal ltyRet,
                             AST.Terminal lname,
                             AST.NonTerminal largs, lstmt])
           f@(AST.FuncDecl _ _ _ args stmt) =
  f { AST.funcWhere = ltyRet':lname':largsID'
    , AST.funcArgs = args'
    , AST.funcCode = addLineStmt lstmt stmt }
  where ltyRet'  = Lexer.getTokenData ltyRet
        lname'   = Lexer.getTokenData lname
        largsID' = map Lexer.getTokenData $ map getIDTree largs
        args'    = map toFuncParam $ zipWith addLineVar largs $ map fromFuncParam args

addLineDecl :: AST.ParseTree -> AST.ASTDecl -> AST.ASTDecl
addLineDecl (AST.NonTerminal ls) (AST.TypeDecl _ tys) =
  AST.TypeDecl (map Lexer.getTokenData $ map getIDTree ls) tys
addLineDecl (AST.NonTerminal ls) (AST.VarDecl _ vars) =
  AST.VarDecl (map Lexer.getTokenData $ map getIDTree ls) (zipWith addLineVar ls vars)

addLineStmt :: AST.ParseTree -> AST.ASTStmt -> AST.ASTStmt
addLineStmt (AST.NonTerminal [AST.NonTerminal ls1, AST.NonTerminal ls2]) (AST.Block decls stmts) =
  AST.Block (zipWith addLineDecl ls1 decls) (zipWith addLineStmt ls2 stmts)
addLineStmt (AST.NonTerminal (optree:ls)) (AST.Expr _ op stmts) =
  AST.Expr (getTerminalData optree) op (zipWith addLineStmt ls stmts)
addLineStmt (AST.NonTerminal [AST.NonTerminal ls1, AST.NonTerminal ls2, AST.NonTerminal ls3, line4]) (AST.For _ forinit forcond foriter forcode) =
  AST.For forline
          (zipWith addLineStmt ls1 forinit)
          forcond'
          (zipWith addLineStmt ls3 foriter)
          (addLineStmt line4 forcode)
  where forcond' = zipWith addLineStmt ls2 forcond
        Just forline  = if null forcond'
                        then Just NoLineInfo
                        else AST.getStmtLine $ last forcond'
addLineStmt (AST.NonTerminal [AST.NonTerminal ls, line]) (AST.While _ whcond whcode) =
  AST.While whline whcond' (addLineStmt line whcode)
  where whcond' = zipWith addLineStmt ls whcond
        Just whline = AST.getStmtLine $ last whcond'
addLineStmt (AST.NonTerminal [line, AST.NonTerminal ls]) (AST.Ap _ fn args) =
  AST.Ap (getTerminalData line) (addLineStmt line fn) (zipWith addLineStmt ls args)
addLineStmt (AST.NonTerminal (line1:line2:(~[line3]))) (AST.If _ con th el) =
  AST.If line con' th' el'
  where con' = addLineStmt line1 con
        th'  = addLineStmt line2 th
        el'  = fmap (addLineStmt line3) el
        Just line = AST.getStmtLine con'
addLineStmt (AST.NonTerminal (line1:(~[line2]))) (AST.Return _ val) =
  AST.Return (getTerminalData line1) (fmap (addLineStmt line2) val)
addLineStmt idtree (AST.Identifier _ name) =
  AST.Identifier (getTerminalData idtree) name
addLineStmt littree (AST.LiteralVal _ lit) =
  AST.LiteralVal (getTerminalData littree) lit
addLineStmt (AST.NonTerminal [line1, line2]) (AST.ArrayRef line ptr ix) =
  AST.ArrayRef line (addLineStmt line1 ptr) (addLineStmt line2 ix)
addLineStmt _ AST.Nop =
  AST.Nop

addLineVar :: AST.ParseTree -> (String, AST.Type, Maybe AST.ASTStmt) -> (String, AST.Type, Maybe AST.ASTStmt)
addLineVar (AST.NonTerminal [_, tytree]) (name, ty, Nothing) =
  (name, addLineTy tytree ty, Nothing)
addLineVar (AST.NonTerminal [_, tytree, inittree]) (name, ty, Just varinit) =
  (name, addLineTy tytree ty, Just $ addLineStmt inittree varinit)

addLineTy :: AST.ParseTree -> AST.Type -> AST.Type
addLineTy (AST.NonTerminal [tytree]) (AST.TPtr t) =
  AST.TPtr (addLineTy tytree t)
addLineTy (AST.NonTerminal [AST.NonTerminal ls, tytree]) (AST.TArray stmts t) =
  AST.TArray (zipWith addLineStmt ls stmts) (addLineTy tytree t)
addLineTy _ t = t

fromFuncParam :: (String, AST.Type) -> (String, AST.Type, Maybe AST.ASTStmt)
fromFuncParam (nam, ty) = (nam, ty, Nothing)

toFuncParam :: (String, AST.Type, Maybe AST.ASTStmt) -> (String, AST.Type)
toFuncParam (nam, ty, _) = (nam, ty)

getIDTree (AST.NonTerminal (AST.Terminal name:_)) = name
getTerminalData (AST.Terminal tok) = Lexer.getTokenData tok
