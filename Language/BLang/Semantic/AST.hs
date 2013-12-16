module Language.BLang.Semantic.AST (
  Type(..),
  Literal(..),
  Operator(..),
  Prog(..),
  FuncDecl(..),
  AST(..),
  getType
) where

import Language.BLang.Data (Assoc)
import Language.BLang.FrontEnd.Parser (Operator(..), Literal(..))

data Type = TInt
          | TFloat
          | TVoid
          | TChar
          | TPtr Type
          | TArray [Integer] Type -- a[][5] is of type (TPtr (TArray [5] _))
          | TArrow [Type] Type -- function of type \Prod [Type] -> Type
          deriving (Show)

data Prog v = Prog { progDecls :: Assoc String v,
                     progFuncs :: Assoc String (FuncDecl v) }

data FuncDecl v = FuncDecl { returnType :: Type,
                             funcArgs :: [(String, Type)],
                             funcCode :: AST v }

data AST v = Block (Assoc String v) [AST v] -- retain block structure, perhaps for scoping issue
           | Expr Type Operator [AST v]
           | ImplicitCast Type Type (AST v)
           | For { forInit :: [AST v],
                   forCond :: [AST v],
                   forIter :: [AST v],
                   forCode :: AST v }
           | While { whileCond :: [AST v],
                     whileCode :: AST v }
           | Ap Type (AST v) [AST v]
           | If (AST v) (AST v) (Maybe (AST v))
           | Return (Maybe (AST v))
           | Identifier Type String
           | LiteralVal Literal
           | Deref Type (AST v) (AST v) -- Deref (Identifier "a") (LiteralVal (IntLiteral 0))
         deriving (Show)

getType :: AST v -> Type
getType (Block _ _) = TVoid
getType (Expr t _ _) = t
getType (For _ _ _ _) = TVoid
getType (While _ _) = TVoid
getType (Ap t _ _) = t
getType (If _ _ _) = TVoid
getType (Return _) = TVoid
getType (Identifier t _) = t
getType (LiteralVal (IntLiteral _)) = TInt
getType (LiteralVal (FloatLiteral _)) = TFloat
getType (LiteralVal (StringLiteral _)) = TPtr TChar
getType (Deref t _ _) = t
