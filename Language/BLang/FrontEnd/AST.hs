module Language.BLang.FrontEnd.AST (
  ParseTree(..),
  Type(..),
  AST,
  ASTTop(..),
  ASTDecl(..),
  ASTStmt(..),
  Operator(..),
  LexToken.Literal(..),
  getStmtLine
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
          deriving (Show)

type AST = [ASTTop]

data ASTTop = VarDeclList [ASTDecl]
            | FuncDecl { funcWhere :: [Line], -- returnType, funcName, args
                         returnType :: Type,
                         funcName :: String,
                         funcArgs :: [(String, Type)],
                         funcCode :: ASTStmt } -- Code :: Blocks
            deriving (Show)

data ASTDecl = TypeDecl [Line] [(String, Type)]
             | VarDecl [Line] [(String, Type, Maybe ASTStmt)]
             deriving (Show)

data Operator = Plus | Minus | Times | Divide | Negate
              | LT   | GT    | LEQ   | GEQ    | EQ | NEQ
              | LOr  | LAnd  | LNot
              | Assign
              deriving (Show, Eq, Ord)

data ASTStmt = Block [ASTDecl] [ASTStmt]
             | Expr Line Operator [ASTStmt]
             | For { forWhere :: Line, -- position of `last . forCond`
                     forInit :: [ASTStmt],
                     forCond :: [ASTStmt], -- relop_expr_list, I don't know why
                     forIter :: [ASTStmt],
                     forCode :: ASTStmt }
             | While { whileWhere :: Line, -- position of `last . whileCond`
                       whileCond :: [ASTStmt], -- mimic that of for statment's
                       whileCode :: ASTStmt }
             | Ap Line ASTStmt [ASTStmt]
             | If Line ASTStmt ASTStmt (Maybe ASTStmt)
             | Return Line (Maybe ASTStmt)
             | Identifier Line String
             | LiteralVal Line LexToken.Literal
             | ArrayRef Line ASTStmt ASTStmt -- ArrayRef (Identifier "a") (LiteralVal (IntLiteral 0))
             | Nop -- for cases like `;;;;;;`
             deriving (Show)

getStmtLine :: ASTStmt -> Maybe Line
getStmtLine (Expr line _ _) = Just line
getStmtLine (Ap line _ _) = Just line
getStmtLine (Identifier line _) = Just line
getStmtLine (LiteralVal line _) = Just line
getStmtLine (ArrayRef line _ _) = Just line
getStmtLine _ = Nothing
