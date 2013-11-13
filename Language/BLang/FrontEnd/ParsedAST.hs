module Language.BLang.FrontEnd.ParsedAST (
  Type(..),
  AST,
  ASTTop(..),
  ASTDecl(..),
  ASTStmt(..),
  Operator(..),
  Lexer.Literal(..)
) where

import qualified Language.BLang.FrontEnd.Lexer as Lexer (Literal(..))

data Type = TInt
          | TFloat
          | TVoid
          | TChar
          | TPtr Type
          | TConst Type
          | TArray [Maybe Int] Type
          | TCustom String
          deriving (Show)

type AST = [ASTTop]

data ASTTop = VarDeclList [ASTDecl]
            | FuncDecl { returnType :: Type,
                         funcName :: String,
                         funcArgs :: [(String, Type)],
                         funcCode :: ASTStmt } -- Code :: Blocks
            deriving (Show)

data ASTDecl = TypeDecl Type [String]
             | VarDecl Type [(String, Maybe ASTStmt)]
             deriving (Show)

data Operator = Add | Mul  | Times | Divide
              | LT  | GT   | LEQ   | GEQ    | EQ | NEQ
              | LOr | LAnd | LNot
              | Assign
              deriving (Show)

data ASTStmt = Block [ASTDecl] [ASTStmt]
             | Expr Operator [ASTStmt]
             | For { forInit :: [ASTStmt],
                     forCond :: [ASTStmt], -- relop_expr_list, I don't know why
                     forIter :: [ASTStmt],
                     forCode :: ASTStmt }
             | While { whileCond :: [ASTStmt], -- mimic that of for statment's
                       whileCode :: ASTStmt }
             | If ASTStmt ASTStmt (Maybe ASTStmt)
             | Return (Maybe ASTStmt)
             | LiteralVal Lexer.Literal
             deriving (Show)
