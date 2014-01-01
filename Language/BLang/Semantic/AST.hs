module Language.BLang.Semantic.AST (
  Type(..),
  Literal(..),
  Operator(..),
  Prog(..),
  FuncDecl(..),
  AST(..),
  getType
) where

import Data.List (intercalate)

import Language.BLang.Data (Assoc)
import Language.BLang.FrontEnd.Parser (Operator(..), Literal(..))
import Language.BLang.Semantic.RawAST (Type(..))

data Prog v = Prog { progDecls :: Assoc String v,
                     progFuncs :: Assoc String (FuncDecl v) }
            deriving (Show)

data FuncDecl v = FuncDecl { returnType :: Type,
                             funcArgs :: [(String, Type)],
                             funcCode :: AST v }

instance Show v => Show (FuncDecl v) where
  show (FuncDecl tyRet args code) =
    "(" ++ intercalate "," (map (\(nam,ty) -> nam ++ ":" ++ showsPrec 11 ty []) args)
    ++ "): " ++ show tyRet ++ "\n" ++ show code ++ "\n"

data AST v = Block (Assoc String v) [AST v] -- retain block structure and declaration sequence
           | For { forInit :: [AST v],
                   forCond :: [AST v],
                   forIter :: [AST v],
                   forCode :: AST v }
           | While { whileCond :: [AST v],
                     whileCode :: AST v }
           | If (AST v) (AST v) (Maybe (AST v))
           | Return (Maybe (AST v))
           | Expr Type Operator [AST v]
           | ImplicitCast Type Type (AST v)
           | Ap Type (AST v) [AST v]
           | Identifier Type String
           | LiteralVal Literal
           | ArrayRef Type (AST v) (AST v) -- ArrayRef (Identifier "a") (LiteralVal (IntLiteral 0))
           | Nop
           deriving (Show)

getType :: AST v -> Type
getType (Block _ _) = TVoid
getType (Expr t _ _) = t
getType (While _ _) = TVoid
getType (Ap t _ _) = t
getType (If _ _ _) = TVoid
getType (Return _) = TVoid
getType (Identifier t _) = t
getType (LiteralVal (IntLiteral _)) = TInt
getType (LiteralVal (FloatLiteral _)) = TFloat
getType (LiteralVal (StringLiteral _)) = TPtr TChar
getType (ArrayRef t _ _) = t
getType Nop = TVoid