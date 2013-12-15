{-# LANGUAGE FlexibleContexts #-}

module Language.BLang.Semantic.DesugarAST where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Applicative

import Language.BLang.Data
import Language.BLang.Error
import Language.BLang.Miscellaneous
import qualified Language.BLang.FrontEnd.Parser as P

-- desugar user-defined type
tyDesugar :: MonadWriter [CompileError] m => P.AST -> m P.AST
tyDesugar ast = liftM fst $ runReaderT (runStateT (mapM tyDeMTop ast) emptyA) emptyA

tyDeMTop :: (MonadReader (Assoc String P.Type) m, MonadState (Assoc String P.Type) m, MonadWriter [CompileError] m)
         => P.ASTTop -> m P.ASTTop
tyDeMTop (P.VarDeclList decls) = liftM P.VarDeclList $ tyDeMDecls decls
tyDeMTop (P.FuncDecl ty name args code) = do
  ty' <- deTy ty
  args' <- mapM (mapsnd deTy) args
  code' <- tyDeMStmt code
  return (P.FuncDecl ty' name args' code')

tyDeMDecls :: (MonadReader (Assoc String P.Type) m, MonadState (Assoc String P.Type) m, MonadWriter [CompileError] m)
           => [P.ASTDecl] -> m [P.ASTDecl]
tyDeMDecls decls = return . wrapList . P.VarDecl =<< tyDeMDecls' decls

tyDeMDecls' :: (MonadReader (Assoc String P.Type) m, MonadState (Assoc String P.Type) m, MonadWriter [CompileError] m)
            => [P.ASTDecl] -> m [(String, P.Type, Maybe P.ASTStmt)]
tyDeMDecls' ((P.VarDecl decls):rest) = do
  decls' <- mapM (map2nd deTy) decls
  rest' <- tyDeMDecls' rest
  return (decls' ++ rest')
tyDeMDecls' ((P.TypeDecl decls):rest) = insertTys decls >> tyDeMDecls' rest

tyDeMStmt :: (MonadReader (Assoc String P.Type) m, MonadState (Assoc String P.Type) m, MonadWriter [CompileError] m)
          => P.ASTStmt -> m P.ASTStmt
tyDeMStmt (P.Block decls stmts) = liftM2 P.Block (tyDeMDecls decls) (mapM tyDeMStmt stmts)
tyDeMStmt for@(P.For _ _ _ code) = do
  code' <- tyDeMStmt code
  return for{ P.forCode = code' }
tyDeMStmt while@(P.While _ code) = do
  code' <- tyDeMStmt code
  return while{ P.whileCode = code' }
tyDeMStmt (P.If con th Nothing) = liftM2 (P.If con) (tyDeMStmt th) (return Nothing)
tyDeMStmt (P.If con th (Just el)) = do
  th' <- tyDeMStmt th
  el' <- tyDeMStmt el
  return (P.If con th' (Just el'))
tyDeMStmt s = return s -- Expr, Ap, Return, Identifier, LiteralVal, ArrayRef, Nop

insertTys :: (MonadReader (Assoc String P.Type) m, MonadState (Assoc String P.Type) m, MonadWriter [CompileError] m)
          => [(String, P.Type)] -> m ()
insertTys tys = do
  tys' <- mapM (mapsnd deTy) tys -- recursive definition is not allowed
  mapM_ insertTy tys'

insertTy :: (MonadReader (Assoc String P.Type) m, MonadState (Assoc String P.Type) m, MonadWriter [CompileError] m)
         => (String, P.Type) -> m ()
insertTy (name, ty) = do
  currScope <- get
  if name `memberA` currScope
    then fail "type name redeclared" -- TODO: line number
    else return ()
  put (insertA name ty currScope)

deTy :: (MonadReader (Assoc String P.Type) m, MonadState (Assoc String P.Type) m, MonadWriter [CompileError] m)
     => P.Type -> m P.Type
deTy (P.TPtr t) = liftM P.TPtr (deTy t)
deTy (P.TArray ixs t) = do
  t' <- deTy t
  case t' of -- merge array types
    (P.TArray ixs' t'') -> return $ P.TArray (ixs ++ ixs') t''
    _ -> return $ P.TArray ixs t'
deTy (P.TCustom name) = do
  currScope <- get
  oldScope <- ask
  case lookupA name currScope <|> lookupA name oldScope of
    Just ty -> return ty
    Nothing -> fail "unknown type name" -- TODO: line number
deTy t = return t -- TInt, TFloat, TVoid, TChar

-- desugar function array type
fnArrDesugar :: P.AST -> P.AST
fnArrDesugar = undefined
