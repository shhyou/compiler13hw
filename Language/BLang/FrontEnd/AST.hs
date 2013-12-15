module Language.BLang.FrontEnd.AST (
  ParseTree(..),
  Type(..),
  AST,
  ASTTop(..),
  ASTDecl(..),
  ASTStmt(..),
  Operator(..),
  LexToken.Literal(..)
) where

import Language.BLang.Data
import qualified Language.BLang.FrontEnd.LexToken as LexToken (Token, Literal(..))

data ParseTree = Terminal (LexToken.Token Line)
               | NonTerminal [ParseTree]

instance Show ParseTree where
  showsPrec d (Terminal tok) = showParen (d > 9) $ ("Terminal " ++) . showsPrec 11 (fmap (const ()) tok)
  showsPrec d (NonTerminal xs) = showParen (d > 9) $ ("NonTerminal " ++) . showsPrec 11 xs

data Type = TInt
          | TFloat
          | TVoid
          | TChar
          | TPtr Type
          | TArray [ASTStmt] Type -- a[][5] is of type (TPtr (TArray [5] _))
          | TCustom String
          deriving (Show, Eq)

type AST = [ASTTop]

data ASTTop = VarDeclList [ASTDecl]
            | FuncDecl { returnType :: Type,
                         funcName :: String,
                         funcArgs :: [(String, Type)],
                         funcCode :: ASTStmt } -- Code :: Blocks
            deriving (Show, Eq)

data ASTDecl = TypeDecl [(String, Type)]
             | VarDecl [(String, Type, Maybe ASTStmt)]
             deriving (Show, Eq)

data Operator = Plus | Minus | Times | Divide | Negate
              | LT   | GT    | LEQ   | GEQ    | EQ | NEQ
              | LOr  | LAnd  | LNot
              | Assign
              deriving (Show, Eq)

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
             | LiteralVal LexToken.Literal
             | ArrayRef ASTStmt ASTStmt -- ArrayRef (Identifier "a") (LiteralVal (IntLiteral 0))
             | Nop -- for cases like `;;;;;;`
             deriving (Show, Eq)
