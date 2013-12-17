module Language.BLang.Homework.Homework4 (semanticCheck) where

import Control.Monad.Writer
import Control.Monad
import Control.Monad.Identity

import Language.BLang.Error
import qualified Language.BLang.FrontEnd.Parser as P
import qualified Language.BLang.FrontEnd.Lexer as Lex
import qualified Language.BLang.Semantic.AST as S

import Language.BLang.Semantic.ConstExprFolding
import Language.BLang.Semantic.DesugarAST
import Language.BLang.Semantic.SymTable
import Language.BLang.Semantic.TypeCheck

semanticCheck :: P.AST-> (S.Prog Var, [CompileError])
semanticCheck ast = runIdentity $ runWriterT $ do
  foldedAST <- constFolding ast
  noTCustomAST <- tyDesugar foldedAST
  let arrptrAST = fnArrDesugar noTCustomAST
  symAST <- buildSymTable arrptrAST
  prog <- typeCheck symAST
  return prog
