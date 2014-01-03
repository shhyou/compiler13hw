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
                             funcVars :: Assoc String Type,
                             funcCode :: [AST v] }

instance Show v => Show (FuncDecl v) where
  show (FuncDecl tyRet args vars code) =
    "(" ++ intercalate "," (map (\(nam,ty) -> nam ++ ":" ++ showsPrec 11 ty []) args)
    ++ "): " ++ show tyRet ++ "\n" ++ show code ++ "\n"

data AST v = For { forInit :: [AST v],
                   forCond :: [AST v],
                   forIter :: [AST v],
                   forCode :: [AST v] }
           | While { whileCond :: [AST v],
                     whileCode :: [AST v] }
           | If (AST v) [AST v] (Maybe [AST v])
           | Return (Maybe (AST v))
           | Expr Type Operator [AST v]
           | ImplicitCast Type Type (AST v)
           | Ap Type (AST v) [AST v]
           | Identifier Type String
           | LiteralVal Literal
           | ArrayRef Type (AST v) (AST v) -- ArrayRef (Identifier "a") (LiteralVal (IntLiteral 0))

instance Show v => Show (AST v) where
  show (Expr ty rator rands) =
    "(" ++ ratorText ++ " " ++ intercalate " " (map show rands) ++ ")"
    where Just ratorText = lookup rator ratorTable
          ratorTable = [(Plus, "+"), (Minus, "-"), (Times, "*"), (Divide, "/"), (Negate, "-"),
                        (LEQ, "<="), (GEQ, ">="), (LOr, "||"), (LAnd, "&&"), (LNot, "!"), (Assign, ":="),
                        (Language.BLang.FrontEnd.Parser.LT, "<"),
                        (Language.BLang.FrontEnd.Parser.GT, ">"),
                        (Language.BLang.FrontEnd.Parser.EQ, "=="), (NEQ, "/=")]
  show (ImplicitCast ty' ty e) =
    "(" ++ showsPrec 11 ty [] ++ "->" ++ showsPrec 11 ty' []
    ++ " " ++ show e ++ ")"
  show (For forinit forcond foriter forcode) =
    "for (" ++ intercalate "," (map show forinit)
    ++ "; " ++ intercalate "," (map show forcond)
    ++ "; " ++ intercalate "," (map show foriter)
    ++ ")\n" ++ showBlocked forcode
  show (While whcond whcode) =
    "while (" ++ show whcond ++ ")\n" ++ showBlocked whcode
  show (Ap ty fn args) =
    "(" ++ show fn ++ concatMap ((' ':) . show) args ++ ")"
  show (If con th el) =
    "if (" ++ show con ++ ")\n"
    ++ showBlocked th
    ++ case el of { Just el' -> "\nelse\n" ++ showBlocked el'; Nothing -> "" }
  show (Return val) =
    "return(" ++ case val of { Just val' -> show val'; Nothing -> "" } ++ ")"
  show (Identifier ty name) =
    name
  show (LiteralVal (IntLiteral n)) =
    show n
  show (LiteralVal (FloatLiteral f)) =
    show f
  show (LiteralVal (StringLiteral s)) =
    show s
  show (ArrayRef ty ref idx) =
    show ref ++ "[" ++ show idx ++ "]"

showBlocked []  = "()"
showBlocked [c] = "  " ++ intercalate "\n  " (split '\n' $ show c) ++ ";"
showBlocked asts =
  "{\n" ++ concatMap (("  " ++) . (++ "\n")) (concatMap (split '\n' . show) asts) ++ "}"

split :: Eq a => a -> [a] -> [[a]]
split c []    = [[]]
split c (c':rest)
  | c == c'   = []:hd:tl
  | otherwise = (c':hd):tl
  where hd:tl = split c rest

getType :: AST v -> Type
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
