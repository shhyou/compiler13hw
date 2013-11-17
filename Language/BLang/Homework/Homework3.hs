{-# LANGUAGE FlexibleContexts #-}

module Language.BLang.Homework.Homework3 (
  AST,
  printAST,
  fromAST
) where

import Control.Monad (mapM, when)
import Control.Monad.State (MonadState, MonadIO, liftIO, runStateT, get, modify)

import qualified Language.BLang.FrontEnd.Parser as Parser

data AST = Node String [AST] deriving Show

freshInt :: MonadState Int m => m Int
freshInt = modify (+1) >> get

printAST :: AST -> IO ()
printAST ast = do
  putStrLn "Diagraph AST"
  putStrLn "{"
  putStrLn "label = \"AST_Graph.gv\""
  fmap fst (runStateT (printAST' ast) (-1))
  putStrLn "}"

printAST' :: (MonadIO m, MonadState Int m) => AST -> m Int
printAST' (Node label children) = do
  curr <- freshInt
  liftIO $ putStrLn $ "node" ++ show curr ++ " [label = \"" ++ label ++ "\"]"
  childrenIDs <- mapM printAST' children
  let printEdge style (src, dst) = do
        liftIO $ putStr $ "node" ++ show src ++ " -> node" ++ show dst
        liftIO $ putStrLn $ " [style = " ++ style ++ "]"
  when (not . null $ childrenIDs) $ do
    printEdge "bold" (curr, head childrenIDs)
    mapM_ (printEdge "dashed") $ zip childrenIDs (tail childrenIDs)
  return curr

emptyNode :: AST
emptyNode = Node "NUL_NODE" []

fromBaseType :: Parser.Type -> AST
fromBaseType (Parser.TPtr typ) = fromBaseType typ
fromBaseType (Parser.TArray _ typ) = fromBaseType typ
fromBaseType typ = normalID (typeRep typ)
  where typeRep Parser.TInt = "int"
        typeRep Parser.TFloat = "float"
        typeRep Parser.TVoid = "void"
        typeRep (Parser.TCustom synonym) = synonym

fromAST :: Parser.AST -> AST
fromAST vs = Node "PROGRAM_NODE" (map fromASTTop vs)

fromASTTop :: Parser.ASTTop -> AST
fromASTTop (Parser.VarDeclList vs) =
  Node "VARIABLE_DECL_LIST_NODE" (map fromASTDecl vs)
fromASTTop (Parser.FuncDecl retTyp name args code) =
  Node "DECLARATION_NODE FUNCTION_DECL" [fromBaseType retTyp, fromID retTyp name, argNode, fromASTStmt code]
  where argNode = Node "PARAM_LIST_NODE" (map declNode args)
        declNode (nam, typ) = Node "DECLARATION_NODE FUNCTION_PARAMETER_DECL" [fromBaseType typ, fromID typ nam]

fromASTDecl :: Parser.ASTDecl -> AST
fromASTDecl (Parser.VarDecl vs) =
  Node "DECLARATION_NODE VARIABLE_DECL" $ (fromBaseType baseTyp):(map fromVarDecl vs)
  where (_, baseTyp, _) = head vs
        fromVarDecl (nam, typ, Nothing) = fromID typ nam
        fromVarDecl (nam, typ, Just init) =
          Node ("IDENTIFIER_NODE " ++ nam ++ " WITH_INIT_ID") [fromASTStmt init]
fromASTDecl (Parser.TypeDecl ts) =
  Node "DECLARATION_NODE TYPE_DECL" $ (fromBaseType baseTyp):(map fromTypeDecl ts)
  where (_, baseTyp) = head ts
        fromTypeDecl (nam, typ) = fromID typ nam

fromASTStmt :: Parser.ASTStmt -> AST
fromASTStmt (Parser.Block decls stmts) = Node "BLOCK_NODE" $ blockDecls decls $ blockStmts stmts []
  where blockDecls [] = id
        blockDecls ds = ((fromASTTop (Parser.VarDeclList ds)):)
        blockStmts [] = id
        blockStmts ss = ((Node "STMT_LIST_NODE" (map fromASTStmt ss)):)
fromASTStmt (Parser.While cond code) =
  Node "STMT_NODE WHILE_STMT" [fromRelopExprList cond, fromASTStmt code]
fromASTStmt (Parser.For init cond iter code) =
  Node "STMT_NODE FOR_STMT" [fromAssignExprList init, fromRelopExprList cond, fromAssignExprList iter, fromASTStmt code]
fromASTStmt (Parser.Expr Parser.Assign stmts) = Node "STMT_NODE ASSIGN_STMT" (map fromASTStmt stmts)
fromASTStmt (Parser.If con th el) =
  Node "STMT_NODE IF_STMT" $
    (fromASTStmt con):(fromASTStmt th):case el of
                                        Just st -> [fromASTStmt st]
                                        Nothing -> []
fromASTStmt (Parser.Ap (Parser.Identifier fun) args) =
  Node "STMT_NODE FUNCTION_CALL_STMT" [normalID fun, fromRelopExprList args]
fromASTStmt (Parser.Return mstmt) = Node "STMT_NODE RETURN_STMT" [mnode]
  where mnode = case mstmt of
                  Nothing -> emptyNode
                  Just stmt -> fromASTStmt stmt
fromASTStmt (Parser.Expr op stmts) = Node ("EXPR_NODE " ++ fromOp op) (map fromASTStmt stmts)
  where fromOp Parser.Plus = "+"
        fromOp Parser.Minus = "-"
        fromOp Parser.Times = "*"
        fromOp Parser.Divide = "/"
        fromOp Parser.Negate = "-"
        fromOp Parser.LT = "<"
        fromOp Parser.GT = ">"
        fromOp Parser.LEQ = "<="
        fromOp Parser.GEQ = ">="
        fromOp Parser.EQ = "=="
        fromOp Parser.NEQ = "!="
        fromOp Parser.LOr = "||"
        fromOp Parser.LAnd = "&&"
        fromOp Parser.LNot = "!"
fromASTStmt (Parser.Identifier nam) = normalID nam
fromASTStmt (Parser.LiteralVal ltr) = Node ("CONST_VALUE_NODE " ++ fromLiteral ltr) []
  where fromLiteral (Parser.StringLiteral str) = str
        fromLiteral (Parser.IntLiteral int) = show int
        formLiteral (Parser.FloatLiteral float) = show float
fromASTStmt e@(Parser.ArrayRef _ _) = fromArrayRef e []
  where fromArrayRef (Parser.ArrayRef (Parser.Identifier nam) idx) lst =
          Node ("IDENTIFIER_NODE " ++ nam ++ " ARRAY_ID") $
          reverse ((fromASTStmt idx):lst)
        fromArrayRef (Parser.ArrayRef innerRef idx) lst =
          fromArrayRef innerRef ((fromASTStmt idx):lst)

fromRelopExprList :: [Parser.ASTStmt] -> AST
fromRelopExprList [] = emptyNode
fromRelopExprList es = Node "NONEMPTY_RELOP_EXPR_LIST_NODE" (map fromASTStmt es)

fromAssignExprList :: [Parser.ASTStmt] -> AST
fromAssignExprList [] = emptyNode
fromAssignExprList es = Node "NONEMPTY_ASSIGN_EXPR_LIST_NODE" (map fromASTStmt es)

normalID :: String -> AST
normalID nam = Node ("IDENTIFIER_NODE " ++ nam ++ " NORMAL_ID") []

fromID :: Parser.Type -> String -> AST
fromID (Parser.TPtr (Parser.TArray dims typ)) nam =
  Node ("IDENTIFIER_NODE " ++ nam ++ " ARRAY_ID") (emptyNode : map fromASTStmt dims)
fromID (Parser.TPtr typ) nam =
  Node ("IDENTIFIER_NODE " ++ nam ++ " ARRAY_ID") [emptyNode]
fromID (Parser.TArray dims typ) nam =
  Node ("IDENTIFIER_NODE " ++ nam ++ " ARRAY_ID") (map fromASTStmt dims)
fromID _ nam = normalID nam
