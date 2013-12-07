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
          | TArray [ASTStmt] Type -- a[][5] is of type (TPtr (TArray [5] _))
          | TCustom String
          deriving (Show)

type AST = [ASTTop]

data ASTTop = VarDeclList [ASTDecl]
            | FuncDecl { returnType :: Type,
                         funcName :: String,
                         funcArgs :: [(String, Type)],
                         funcCode :: ASTStmt } -- Code :: Blocks
            deriving (Show)

data ASTDecl = TypeDecl [(String, Type)]
             | VarDecl [(String, Type, Maybe ASTStmt)]
             deriving (Show)

data Operator = Plus | Minus | Times | Divide | Negate
              | LT   | GT    | LEQ   | GEQ    | EQ | NEQ
              | LOr  | LAnd  | LNot
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
             | Ap ASTStmt [ASTStmt]
             | If ASTStmt ASTStmt (Maybe ASTStmt)
             | Return (Maybe ASTStmt)
             | Identifier String
             | LiteralVal Lexer.Literal
             | ArrayRef ASTStmt ASTStmt -- ArrarRef (Identifier "a") (LiteralVal (IntLiteral 0))
             | Nop -- for cases like `;;;;;;`
             deriving (Show)
