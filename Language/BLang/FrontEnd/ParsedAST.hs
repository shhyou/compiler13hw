module Language.BLang.FrontEnd.ParsedAST (
  AST(..),
  Decl(..),
  Type(..)
) where

data AST = Program [AST]
         | VarDeclList [Decl]
         | FunctDecl Type String [(String, Type)]
         | Block
         | Stmt
         | BinaryOp
         | UnaryOp
         | ConstVal
         | AssignExprList
         | RelOpExprList
         deriving (Show)

data Decl = TypeDecl Type [String]
          | VarDecl Type [String]
          deriving (Show)

data Type = TInt
          | TFloat
          | TVoid
          | TChar
          | TPtr Type
          | TConst Type
          | TArray [Maybe Int] Type
          | TCustom String
          deriving (Show)
