module Main (main) where

import qualified Language.BLang.FrontEnd.Lexer as Lexer
import qualified Language.BLang.FrontEnd.Parser as Parser

import qualified Language.BLang.FrontEnd.ParseMonad as ParseMonad -- for testing

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
  case lexStream input of
    Left parseError -> putStrLn "[ERROR]" >> putStrLn (show parseError)
    Right stream    -> print stream



plzPrintNode :: String -> [a] -> IO (Int) -> IO (Int)
plzPrintNode label xs root = do
  r <- root
  putStrLn $ "node" ++ show r ++ " [label = \"" ++ label ++ "\"]"
  liftM snd $ foldl folder (root, return (r+1)) xs
    where
      folder pr n =
          r', u' <- pr
          let u'' = printNode n u'
          print $ "node" ++ show r' ++ " -> node" ++ show u' ++ " [style = "
          print if r' == r then "bold" else "dashed"
          putStrLn "]"
          return (u', u'')

printNode :: a -> IO Int -> IO Int
printNode (xs :: AST) _ = do
  putStrLn "Diagraph AST"
  putStrLn "{"
  putStrLn "label = \"AST_Graph.gv\""
  let res = plzPrintNode "PROGRAM_NODE" xs (return 0)
  putStrLn "}"
  res

printNode [] = plzPrintNode "NUL_NODE" []

printNode (VarDecl xs) = plzPrintNode "DECLARATION_NODE VARIABLE_DECL" (baseType xs : ys)
    where
      ys = map (\(s, t, d) -> toID s t d) xs
      baseType = baseTypeOf . (\(_, t, _) -> t) . head

printNode (TypeDecl xs) = plzPrintNode "DECLARATION_NODE TYPE_DECL" (baseType xs : ys)
    where
      ys = map (NormalID . fst) xs
      baseType = baseTypeOf . snd . head

printNode (FuncDecl t name args code) = plzPrintNode "DECLARATION_NODE FUNCTION_DECL" xs
    where xs = [t, NormalID name, map ArgDecl args, code]

printNode (ArgDecl (fname, ftype)) =
    plzPrintNode "DECLARATION_NODE FUNCTION_PARAMETER_DECL" [ftype, NormalID fname]

printNode (Identifier str) = printNode (NormalID str)

printNode (NormalID str) = plzPrintNode ("IDENTIFIER_NODE " ++ str ++ " NORMAL_ID") []
printNode (ArrayID str xs) = plzPrintNode ("IDENTIFIER_NODE " ++ str ++ " ARRAY_ID") xs
printNode (WithInitID str stmt) = plzPrintNode ("IDENTIFIER_NDOE " ++ str ++ " WITH_INIT_ID") stmt

printNode (xs :: ArgList) = plzPrintNode "PARAM_LIST_NODE" xs

printNode (Block decls stmts) = plzPrintNode "BLOCK_NODE" children
    where children = map (\(a, b) => a $ b) $ filter (not . null . snd) $ zip [BDecls, BStmts] [decls, stmts]

printNode (VarDeclList xs) = plzPrintNode "VARIABLE_DECL_LIST_NODE" xs

printNode (BStmts stmts) = plzPrintNode "STMT_LIST_NODE" stmts

printNode (While cond code) = plzPrintNode "STMT_NODE WHILE_STMT" [cond, code]
printNode (For init cond iter code) = plzPrintNode "STMT_NODE FOR_STMT" xs
    where xs = [ForAssign init, ForCtrl cond, ForAssign iter, code]
printNode (Expr Assign stmts) = plzPrintNode "STMT_NODE ASSIGN_STMT" stmts
printNode (If cond astmts mbstmts) = plzPrintNode "STMT_NODE IF_STMT" [cond, astmts, fromMaybe [] mbstmts]
printNode (Ap func args) = plzPrintNode "STMT_NODE FUNCTION_CALL_STMT" [func, FuncCallArgs args]
printNode (Return mstmt) =
    if isNothing mstmt
    then id
    else plzPrintNode "STMT_NODE RETURN_STMT" (maybeToList mstmt)

printNode (Expr op stmts) = plzPrintNode ("EXPR_NODE " ++ show op) stmts
    where
      show Plus = "+"
      show Minus = "-"
      show Times = "*"
      show Divide = "/"
      show LT = "<"
      show GT = ">"
      show LEQ = "<="
      show GEQ = ">="
      show EQ = "=="
      show NEQ = "!="
      show LOr = "||"
      show LAnd = "&&"
      show LNot = "!"

printNode (LiteralVal dfq) = plzPrintNode ("CONST_VALUE_NODE" ++ show literal) []
    where
      show StringLiteral str = "\"" ++ str ++ "\""
      show _ = show

data ForCtrl = ForAssign [ASTStmt] | ForRelop [ASTStmt]

printNode (ForAssign []) = printNode []
printNode (ForAssign xs) = plzPrintNode "NONEMPTY_ASSIGN_EXPR_LIST_NODE" xs

printNode (ForRelop []) = printNode []
printNode (ForRelop xs) = plzPrintNode "NONEMPTY_RELOP_EXPR_LIST_NODE" xs


data ArgList = ArgList [ArgDecl]
data ArgDecl = ArgDecl (String, Type)

data BlockChild = BDecls [ASTDecl] | BStmts [ASTStmt]

printNode BDecls = printNode . VarDeclList


data ID = NormalID String | ArrayID String [ASTStmt] | WithInitID String ASTStmt

toID :: String -> Type -> Maybe ASTStmt -> ID
toID str (TArray xs) Nothing  _ = ID str xs
toID str _ (Just stmt) = WithInitID str stmt
toID str _ _ = NormalID str


baseTypeOf :: Type -> Type
baseTypeOf TPtr = baseTypeOf
baseTypeOf TConst = baseTypeOf
baseTypeOf TArray _ = baseTypeOf
baseTypeOf def = def
