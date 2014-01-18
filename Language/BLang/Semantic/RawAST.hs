module Language.BLang.Semantic.RawAST (
  Type(..),
  Literal(..),
  Operator(..),
  Prog(..),
  FuncDecl(..),
  AST(..),
  Var(..),
  getType
) where

import Data.List (intercalate)

import Language.BLang.Data (Assoc, Line(), (!))
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

data Var = Var { varType :: Type, varLine :: Line, varInit :: Maybe (AST Var) }

data Prog v = Prog { progDecls :: [(String, v)],
                     progFuncs :: Assoc String (FuncDecl v) }
            deriving (Show)

data FuncDecl v = FuncDecl { returnType :: Type,
                             funcEnv :: Assoc String v,
                             funcArgs :: [(String, Type)],
                             funcCode :: AST v }

data AST v = Block [(String, v)] [AST v] -- retain block structure and declaration sequence
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
           | ArrayRef Type Line (AST v) (AST v) -- ArrayRef (Identifier "a") (LiteralVal (IntLiteral 0))
           | Nop

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
getType (ArrayRef t _ _ _) = t
getType Nop = TVoid

getASTLine :: AST v -> Maybe Line
getASTLine (Expr _ line _ _) = Just line
getASTLine (ImplicitCast _ _ ast) = getASTLine ast
getASTLine (Ap _ line _ _) = Just line
getASTLine (Identifier _ line _) = Just line
getASTLine (LiteralVal line _) = Just line
getASTLine (ArrayRef _ line _ _) = Just line
getASTLine _ = Nothing -- Block, For, While, If

showBlocked c@(Block _ _) = show c
showBlocked c             = "  " ++ intercalate "\n  " (split '\n' $ show c) ++ ";"

split :: Eq a => a -> [a] -> [[a]]
split c []    = [[]]
split c (c':rest)
  | c == c'   = []:hd:tl
  | otherwise = (c':hd):tl
  where hd:tl = split c rest

instance Show Var where
  show (Var ty _ varinit) =
    show ty ++ case varinit of
      Nothing -> ""
      Just varinit' -> " := " ++ show varinit'

instance Show v => Show (FuncDecl v) where
  show (FuncDecl tyRet _ args code) =
    "(" ++ intercalate "," (map (\(nam,ty) -> nam ++ ":" ++ showsPrec 11 ty []) args)
    ++ "): " ++ show tyRet ++ "\n" ++ show code ++ "\n"

instance Show v => Show (AST v) where
  show (Block tbl asts) =
    "{\n  " ++ show tbl ++ "\n"
    ++ concatMap (("  " ++) . (++ "\n")) (concatMap (split '\n' . show) asts)
    ++ "}"
  show (Expr ty _ rator rands) =
    "(" ++ ratorText ++ " " ++ intercalate " " (map show rands) ++ ")"
    where Just ratorText = lookup rator ratorTable
          ratorTable = [(Plus, "+"), (Minus, "-"), (Times, "*"), (Divide, "/"), (Negate, "-"),
                        (LEQ, "<="), (GEQ, ">="), (LOr, "||"), (LAnd, "&&"), (LNot, "!"), (Assign, ":="),
                        (Language.BLang.FrontEnd.Parser.LT, "<"),
                        (Language.BLang.FrontEnd.Parser.GT, ">"),
                        (Language.BLang.FrontEnd.Parser.EQ, "=="), (NEQ, "/=")]
    -- "(" ++ show rator ++ ":" ++ showsPrec 11 ty [] ++ " "
    -- ++ intercalate " " (map show rands) ++ ")"
  show (ImplicitCast ty' ty e) =
    "(" ++ showsPrec 11 ty [] ++ "->" ++ showsPrec 11 ty' []
    ++ " " ++ show e ++ ")"
  show (For _ forinit forcond foriter forcode) =
    "for (" ++ intercalate "," (map show forinit)
    ++ "; " ++ intercalate "," (map show forcond)
    ++ "; " ++ intercalate "," (map show foriter)
    ++ ")\n" ++ showBlocked forcode
  show (While _ whcond whcode) =
    "while (" ++ show whcond ++ ")\n" ++ showBlocked whcode
  show (Ap ty _ fn args) =
    "(" ++ show fn ++ concatMap ((' ':) . show) args ++ ")"
    -- "(Ap:" ++ showsPrec 11 ty [] ++ " " ++ show fn ++ " ["
    -- ++ intercalate "," (map show args) ++ "])"
  show (If _ con th el) =
    "if (" ++ show con ++ ")\n"
    ++ showBlocked th
    ++ case el of { Just el' -> "\nelse\n" ++ showBlocked el'; Nothing -> "" }
  show (Return _ val) =
    "return(" ++ case val of { Just val' -> show val'; Nothing -> "" } ++ ")"
  show (Identifier ty _ name) =
    name
    -- "(" ++ name ++ ":" ++ show ty ++ ")"
  show (LiteralVal _ (IntLiteral n)) =
    show n
  show (LiteralVal _ (FloatLiteral f)) =
    show f
  show (LiteralVal _ (StringLiteral s)) =
    show s
    -- "(LiteralVal " ++ show lit ++ ")"
  show (ArrayRef ty _ ref idx) =
    show ref ++ "[" ++ show idx ++ "]"
    -- "(ArrayRef:" ++ showsPrec 11 ty [] ++ " " ++ show ref ++ "[" ++ show idx ++ "])"
  show Nop =
    "()"
