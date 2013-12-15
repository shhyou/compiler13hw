module Language.BLang.Semantic.AST (
  Type(..),
  Literal(..)
)where

import Language.BLang.FrontEnd.Lexer (Literal(..))

data Type = TInt
          | TFloat
          | TVoid
          | TChar
          | TPtr Type
          | TArray [Integer] Type -- a[][5] is of type (TPtr (TArray [5] _))
          deriving (Show)

data Operator = Plus | Minus | Times | Divide | Negate
              | LT   | GT    | LEQ   | GEQ    | EQ | NEQ
              | LOr  | LAnd  | LNot
              | Assign
              deriving (Show)

data AST = Block [AST]
         | Expr Type Operator [AST]
         | ImplicitCast Type Type AST
         | For { forInit :: [AST],
                 forCond :: [AST],
                 forIter :: [AST],
                 forCode :: AST }
         | While { whileCond :: [AST],
                   whileCode :: AST }
         | Ap Type AST [AST]
         | If AST AST (Maybe AST)
         | Return (Maybe AST)
         | Identifier String
         | LiteralVal Type Literal
         | Deref Type AST AST -- Deref (Identifier "a") (LiteralVal (IntLiteral 0))
         deriving (Show)
