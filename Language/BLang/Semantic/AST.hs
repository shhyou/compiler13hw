module Language.BLang.Semantic.AST (
  Type(..),
  Literal(..),
  Operator(..),
  Prog(..),
  FuncDecl(..),
  AST(..),
  getType
) where

import Language.BLang.Data (Assoc, Line())
import Language.BLang.FrontEnd.Parser (Operator(..), Literal(..))

data Type = TInt
          | TFloat
          | TVoid
          | TChar
          | TPtr Type
          | TArray [Integer] Type -- a[][5] is of type (TPtr (TArray [5] _))
          | TArrow [Type] Type -- function of type \Prod [Type] -> Type
          | TTypeSyn -- type synonym, for identifiers like `typdef int int_t;`
          deriving (Show, Eq)

data Prog v = Prog { progDecls :: Assoc String v,
                     progFuncs :: Assoc String (FuncDecl v) }
            deriving (Show)

data FuncDecl v = FuncDecl { returnType :: Type,
                             funcArgs :: [(String, Type)],
                             funcCode :: AST v }
                deriving (Show)

data AST v = Block (Assoc String v) [AST v] -- retain block structure, perhaps for scoping issue
           | Expr Type Line Operator [AST v]
           | ImplicitCast Type Type (AST v)
           | For { forLine :: Line,
                   forInit :: [AST v],
                   forCond :: [AST v],
                   forIter :: [AST v],
                   forCode :: AST v }
           | While { whileLine :: Line,
                     whileCond :: [AST v],
                     whileCode :: AST v }
           | Ap Type Line (AST v) [AST v]
           | If Line (AST v) (AST v) (Maybe (AST v))
           | Return Line (Maybe (AST v))
           | Identifier Type Line String
           | LiteralVal Line Literal
           | Deref Type Line (AST v) (AST v) -- Deref (Identifier "a") (LiteralVal (IntLiteral 0))
           deriving (Show)

getType :: AST v -> Type
getType (Block _ _) = TVoid
getType (Expr t _ _ _) = t
getType (For _ _ _ _ _) = TVoid
getType (While _ _ _) = TVoid
getType (Ap t _ _ _) = t
getType (If _ _ _ _) = TVoid
getType (Return _ _) = TVoid
getType (Identifier t _ _) = t
getType (LiteralVal _ (IntLiteral _)) = TInt
getType (LiteralVal _ (FloatLiteral _)) = TFloat
getType (LiteralVal _ (StringLiteral _)) = TPtr TChar
getType (Deref t _ _ _) = t

getASTLine :: AST v -> Maybe Line
getASTLine (Expr _ line _ _) = Just line
getASTLine (ImplicitCast _ _ ast) = getASTLine ast
getASTLine (Ap _ line _ _) = Just line
getASTLine (Identifier _ line _) = Just line
getASTLine (LiteralVal line _) = Just line
getASTLine (Deref _ line _ _) = Just line
getASTLine _ = Nothing -- Block, For, While, If
