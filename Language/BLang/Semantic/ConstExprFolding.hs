{-# LANGUAGE FlexibleContexts #-}

module Language.BLang.Semantic.ConstExprFolding (
  constFolding
) where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.State
import Control.Applicative

import Language.BLang.Error
import qualified Language.BLang.FrontEnd.Parser as P

-- evaluate values in the types
constFolding :: MonadWriter [CompileError] m
             => P.AST -> m P.AST
constFolding = mapM foldTop

foldTop :: MonadWriter [CompileError] m
        => P.ASTTop -> m P.ASTTop
foldTop (P.VarDeclList decls) =
  liftM P.VarDeclList $ mapM foldDecl decls
foldTop f@(P.FuncDecl _ _ args stmt) = do
  args' <- mapM foldFuncArg args
  stmt' <- foldStmt stmt
  return $ f{ P.funcArgs = args', P.funcCode = stmt' }

foldDecl :: MonadWriter [CompileError] m
         => P.ASTDecl -> m P.ASTDecl
foldDecl (P.TypeDecl decls) = undefined
foldDecl (P.VarDecl decls) = undefined

foldFuncArg :: MonadWriter [CompileError] m
            => (String, P.Type) -> m (String, P.Type)
foldFuncArg = undefined

foldStmt :: MonadWriter [CompileError] m
         => P.ASTStmt -> m P.ASTStmt
foldStmt = undefined
