{-# LANGUAGE ExistentialQuantification #-}

module Main (main) where

import Data.Maybe (maybeToList)
import Data.Foldable (foldlM)
import Control.Monad (liftM)

import qualified Language.BLang.FrontEnd.Lexer as Lexer
import qualified Language.BLang.FrontEnd.Parser as Parser

import qualified Language.BLang.FrontEnd.ParseMonad as ParseMonad -- for testing
import qualified Language.BLang.Debug.ParserAST as Debug.ParserAST (printAST)

lexStream :: String -> Either ParseMonad.ParseError [Lexer.Token]
lexStream = ParseMonad.runParser lexStream'
  where lexStream' :: ParseMonad.Parser [Lexer.Token]
        lexStream' = Lexer.lexer $ \token ->
                      case token of
                        Lexer.EOF -> return [Lexer.EOF]
                        _         -> return . (token:) =<< lexStream'

main :: IO ()
main = do
  input <- getContents
{-
  case lexStream input of
    Left parseError -> putStrLn "[ERROR] [LEXER]" >> putStrLn (show parseError)
    Right stream    -> putStrLn "[TOKEN]" >> mapM_ (\a -> putStr "  " >> print a) stream
-}
  case Parser.parse input of
    Left parseError -> putStrLn "[ERROR] [PARSER]" >> putStrLn (show parseError)
    Right ast       -> putStrLn "[AST]" >> Debug.ParserAST.printAST ast


data Printable = forall a. ASTAll a => Packed a
instance ASTAll Printable where
    printNode (Packed n) = printNode n

printAST :: Parser.AST -> IO ()
printAST xs = do
  putStrLn "Diagraph AST"
  putStrLn "{"
  putStrLn "label = \"AST_Graph.gv\""
  plzPrintNode "PROGRAM_NODE" xs (return 0)
  putStrLn "}"

plzPrintNode :: ASTAll a => String -> [a] -> IO Int -> IO Int
plzPrintNode label xs root = do
  r <- root
  putStrLn $ "node" ++ show r ++ " [label = \"" ++ label ++ "\"]"
  liftM snd $ foldlM (folder r) (r, r+1) xs
    where
      folder r (r', u') n = do
        u'' <- printNode n (return u')
        print $ "node" ++ show r' ++ " -> node" ++ show u' ++ " [style = "
        print $ if r' == r then "bold" else "dashed"
        putStrLn "]"
        return (u', u'')


class ASTAll a where
    printNode :: a -> IO Int -> IO Int

newtype ParserAST = ParserAST Parser.AST
instance ASTAll ParserAST where
    printNode (ParserAST []) = plzPrintNode "NUL_NODE" []

instance ASTAll Parser.ASTTop where
    printNode (Parser.VarDeclList xs) = plzPrintNode "VARIABLE_DECL_LIST_NODE" xs
    printNode (Parser.FuncDecl t name args code) = plzPrintNode "DECLARATION_NODE FUNCTION_DECL" xs
        where xs = [Packed t, Packed (NormalID name), Packed $ map ArgDecl args, Packed code]

instance ASTAll Parser.ASTDecl where
    printNode (Parser.VarDecl xs) = plzPrintNode "DECLARATION_NODE VARIABLE_DECL" zs
        where
          ys = map (\(s, t, d) -> toID s t d) xs
          baseType = baseTypeOf . (\(_, t, _) -> t) . head
          zs = Packed (baseType xs) : map Packed ys
    printNode (Parser.TypeDecl xs) = plzPrintNode "DECLARATION_NODE TYPE_DECL" zs
        where
          ys = map (NormalID . fst) xs
          baseType = baseTypeOf . snd . head
          zs = Packed (baseType xs) : map Packed ys

instance ASTAll Parser.ASTStmt where
    printNode (Parser.Identifier str) = printNode (NormalID str)

    printNode (Parser.Block decls stmts) = plzPrintNode "BLOCK_NODE" children
        where
          achild = if null stmts then [] else [BStmts stmts]
          children = if null decls then achild else (BDecls decls : achild)

    printNode (Parser.While cond code) = plzPrintNode "STMT_NODE WHILE_STMT" [Packed cond, Packed code]
    printNode (Parser.For init cond iter code) = plzPrintNode "STMT_NODE FOR_STMT" xs
        where xs = [Packed (ForAssign init), Packed (ForRelop cond), Packed (ForAssign iter), Packed code]
    printNode (Parser.Expr Parser.Assign stmts) = plzPrintNode "STMT_NODE ASSIGN_STMT" stmts
    printNode (Parser.If cond astmts mbstmts) = plzPrintNode "STMT_NODE IF_STMT" xs
        where
          bstmts = maybeToList mbstmts
          xs = [Packed cond, Packed astmts, Packed bstmts]
    printNode (Parser.Ap func args) = plzPrintNode "STMT_NODE FUNCTION_CALL_STMT" [Packed func, Packed args]
    printNode (Parser.Return mstmt) =
        case mstmt of
          Nothing -> id
          Just stmt -> plzPrintNode "STMT_NODE RETURN_STMT" [stmt]

    printNode (Parser.Expr op stmts) = plzPrintNode ("EXPR_NODE " ++ show op) stmts
        where
          show Parser.Plus = "+"
          show Parser.Minus = "-"
          show Parser.Times = "*"
          show Parser.Divide = "/"
          show Parser.Negate = "-"
          show Parser.LT = "<"
          show Parser.GT = ">"
          show Parser.LEQ = "<="
          show Parser.GEQ = ">="
          show Parser.EQ = "=="
          show Parser.NEQ = "!="
          show Parser.LOr = "||"
          show Parser.LAnd = "&&"
          show Parser.LNot = "!"

    printNode (Parser.LiteralVal literal) = plzPrintNode ("CONST_VALUE_NODE" ++ showl literal) []
        where
          showl (Parser.StringLiteral str) = "\"" ++ str ++ "\""
          showl (Parser.IntLiteral els) = show els
          showl (Parser.FloatLiteral els) = show els

data ForCtrl = ForAssign [Parser.ASTStmt]
             | ForRelop [Parser.ASTStmt]

instance ASTAll ForCtrl where
    printNode (ForAssign []) = printNode []
    printNode (ForAssign xs) = plzPrintNode "NONEMPTY_ASSIGN_EXPR_LIST_NODE" xs

    printNode (ForRelop []) = printNode []
    printNode (ForRelop xs) = plzPrintNode "NONEMPTY_RELOP_EXPR_LIST_NODE" xs

data ArgList = ArgList [Parser.ASTStmt]

instance ASTAll ArgList where
    printNode (ArgList xs) = plzPrintNode "PARAM_LIST_NODE" xs

data ArgDecl = ArgDecl (String, Parser.Type)

instance ASTAll ArgDecl where
    printNode (ArgDecl (fname, ftype)) =
        plzPrintNode "DECLARATION_NODE FUNCTION_PARAMETER_DECL" [Packed ftype, Packed (NormalID fname)]

data BlockChild = BDecls [Parser.ASTDecl] | BStmts [Parser.ASTStmt]

instance ASTAll BlockChild where
    printNode (BDecls decls) = printNode . Parser.VarDeclList $ decls
    printNode (BStmts stmts) = plzPrintNode "STMT_LIST_NODE" stmts

data ID = NormalID String | ArrayID String [Parser.ASTStmt] | WithInitID String Parser.ASTStmt

instance ASTAll ID where
    printNode (NormalID str) = plzPrintNode ("IDENTIFIER_NODE " ++ str ++ " NORMAL_ID") []
    printNode (ArrayID str xs) = plzPrintNode ("IDENTIFIER_NODE " ++ str ++ " ARRAY_ID") xs
    printNode (WithInitID str stmt) = plzPrintNode ("IDENTIFIER_NDOE " ++ str ++ " WITH_INIT_ID") [stmt]

toID :: String -> Parser.Type -> Maybe Parser.ASTStmt -> ID
toID str (Parser.TArray xs _) Nothing  = ArrayID str xs
toID str _ (Just stmt) = WithInitID str stmt
toID str _ _ = NormalID str


baseTypeOf :: Parser.Type -> Parser.Type
baseTypeOf (Parser.TPtr t) = baseTypeOf t
-- no const type for now. baseTypeOf Parser.TConst = baseTypeOf
baseTypeOf (Parser.TArray _ t) = baseTypeOf t
baseTypeOf def = def
