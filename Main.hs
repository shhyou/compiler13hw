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
    Right ast       -> printAST ast

type Node = IO Int -> IO Int

printAST :: Parser.AST -> IO ()
printAST xs = do
  putStrLn "Digraph AST"
  putStrLn "{"
  putStrLn "label = \"AST_Graph.gv\""
  printChildren "PROGRAM_NODE" (map makeNode xs) (return 0)
  putStrLn "}"

printChildren :: String -> [Node] -> Node
printChildren label xs root = do
  r <- root
  putStrLn $ "node" ++ show r ++ " [label = \"" ++ label ++ "\"]"
  liftM snd $ foldlM (folder r) (r, r+1) xs
    where
      folder r (r', u') node = do
        u'' <- node (return u')
        putStr $ "node" ++ show r' ++ " -> node" ++ show u' ++ " [style = "
        putStr $ if r' == r then "bold" else "dashed"
        putStrLn "]"
        return (u', u'')


class ASTAll a where
    makeNode :: a -> Node

instance ASTAll [a] where
    makeNode [] = printChildren "NUL_NODE" []

newtype ParserAST = ParserAST Parser.AST
instance ASTAll ParserAST where
    makeNode (ParserAST []) = printChildren "NUL_NODE" []

data FuncDefParas = FuncDefParas [ArgDecl]
instance ASTAll FuncDefParas where
    makeNode (FuncDefParas xs) = printChildren "PARAM_LIST_NODE" (map makeNode xs)  -- NONEMPTY_RELOP_EXPR_LIST_NODE

instance ASTAll Parser.ASTTop where
    makeNode (Parser.VarDeclList xs) = printChildren "VARIABLE_DECL_LIST_NODE" (map makeNode xs)
    makeNode (Parser.FuncDecl t name args code) = printChildren "DECLARATION_NODE FUNCTION_DECL" xs
        where
          params = FuncDefParas $ map ArgDecl args
          xs = [makeNode (typeToID t), makeNode (NormalID name), makeNode params, makeNode code]

instance ASTAll Parser.ASTDecl where
    makeNode (Parser.VarDecl xs) = printChildren "DECLARATION_NODE VARIABLE_DECL" zs
        where
          ys = map (\(s, t, d) -> toID s t d) xs
          baseTypeID = baseTypeToID . baseTypeOf . (\(_, t, _) -> t) . head
          zs = makeNode (baseTypeID xs) : map makeNode ys
    makeNode (Parser.TypeDecl xs) = printChildren "DECLARATION_NODE TYPE_DECL" zs
        where
          ys = map (NormalID . fst) xs
          baseTypeID = baseTypeToID . snd . head
          zs = makeNode (baseTypeID xs) : map makeNode ys

instance ASTAll Parser.ASTStmt where
    makeNode (Parser.Identifier str) = makeNode (NormalID str)

    makeNode (Parser.Block decls stmts) = printChildren "BLOCK_NODE" (map makeNode children)
        where
          achild = if null stmts then [] else [BStmts stmts]
          children = if null decls then achild else (BDecls decls : achild)

    makeNode (Parser.While cond code) = printChildren "STMT_NODE WHILE_STMT" [makeNode (head cond), makeNode code]
    makeNode (Parser.For init cond iter code) = printChildren "STMT_NODE FOR_STMT" xs
        where xs = [makeNode (ForAssign init), makeNode (ForRelop cond), makeNode (ForAssign iter), makeNode code]
    makeNode (Parser.Expr Parser.Assign stmts) = printChildren "STMT_NODE ASSIGN_STMT" (map makeNode stmts)
    makeNode (Parser.If cond astmt mbstmt) = printChildren "STMT_NODE IF_STMT" xs
        where
          bstmt = maybeToList mbstmt
          xs = [makeNode cond, makeNode astmt, if null bstmt then makeNode [] else makeNode (head bstmt)]
    makeNode (Parser.Ap func args) =
        printChildren "STMT_NODE FUNCTION_CALL_STMT" [makeNode func, makeNode (ArgList args)]
    makeNode (Parser.Return mstmt) =
        case mstmt of
          Nothing -> id
          Just stmt -> printChildren "STMT_NODE RETURN_STMT" [makeNode stmt]

    makeNode (Parser.Expr op stmts) = printChildren ("EXPR_NODE " ++ show op) (map makeNode stmts)
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

    makeNode (Parser.LiteralVal literal) = printChildren ("CONST_VALUE_NODE " ++ showl literal) []
        where
          showl (Parser.StringLiteral els) = els
          showl (Parser.IntLiteral els) = show els
          showl (Parser.FloatLiteral els) = show els

    makeNode (Parser.ArrayRef inner dim) = printChildren ("IDENTIFIER_NODE " ++ aid ++ " ARRAY_ID") children
        where
          sub (Parser.ArrayRef i d) acc = sub i (makeNode d : acc)
          sub (Parser.Identifier i) acc = (i, acc)
          (aid, children) = sub inner [makeNode dim]

    makeNode (Parser.Nop) = printChildren ("NUL_NODE") []

data ForCtrl = ForAssign [Parser.ASTStmt]
             | ForRelop [Parser.ASTStmt]

instance ASTAll ForCtrl where
    makeNode (ForAssign []) = makeNode []
    makeNode (ForAssign xs) = printChildren "NONEMPTY_ASSIGN_EXPR_LIST_NODE" (map makeNode xs)

    makeNode (ForRelop []) = makeNode []
    makeNode (ForRelop xs) = printChildren "NONEMPTY_RELOP_EXPR_LIST_NODE" (map makeNode xs)

data ArgList = ArgList [Parser.ASTStmt]

instance ASTAll ArgList where
    makeNode (ArgList xs) = printChildren "NONEMPTY_RELOP_EXPR_LIST_NODE" (map makeNode xs)

data ArgDecl = ArgDecl (String, Parser.Type)

instance ASTAll ArgDecl where
    makeNode (ArgDecl (fname, ftype)) =
        printChildren "DECLARATION_NODE FUNCTION_PARAMETER_DECL" [makeNode typeid, makeNode nameid]
            where
              typeid = baseTypeToID . baseTypeOf $ ftype
              nameid = toID fname ftype Nothing

data BlockChild = BDecls [Parser.ASTDecl] | BStmts [Parser.ASTStmt]

instance ASTAll BlockChild where
    makeNode (BDecls decls) = makeNode . Parser.VarDeclList $ decls
    makeNode (BStmts stmts) = printChildren "STMT_LIST_NODE" (map makeNode stmts)

data ID = NormalID String | ArrayID String [Parser.ASTStmt] | WithInitID String Parser.ASTStmt

instance ASTAll ID where
    makeNode (NormalID str) = printChildren ("IDENTIFIER_NODE " ++ str ++ " NORMAL_ID") []
    makeNode (ArrayID str xs) = printChildren ("IDENTIFIER_NODE " ++ str ++ " ARRAY_ID") (map makeNode xs)
    makeNode (WithInitID str stmt) = printChildren ("IDENTIFIER_NODE " ++ str ++ " WITH_INIT_ID") [makeNode stmt]

toID :: String -> Parser.Type -> Maybe Parser.ASTStmt -> ID
toID str (Parser.TArray xs _) Nothing = ArrayID str xs
toID str (Parser.TPtr (Parser.TArray xs _)) Nothing = ArrayID str (Parser.Nop : xs) -- using Nop for NUL_NODE
toID str _ (Just stmt) = WithInitID str stmt
toID str _ _ = NormalID str

baseTypeToID :: Parser.Type -> ID
baseTypeToID = NormalID . show
    where
      show Parser.TInt = "int"
      show Parser.TFloat = "float"
      show Parser.TVoid = "void"
      show (Parser.TCustom str) = str

typeToID :: Parser.Type -> ID
typeToID (Parser.TArray xs t) = ArrayID tname xs
    where (NormalID tname) = baseTypeToID t
typeToID els = baseTypeToID els

baseTypeOf :: Parser.Type -> Parser.Type
baseTypeOf (Parser.TPtr t) = baseTypeOf t
baseTypeOf (Parser.TArray _ t) = baseTypeOf t
baseTypeOf def = def
