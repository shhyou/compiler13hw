{-# LANGUAGE FlexibleContexts #-}

module Language.BLang.Semantic.DesugarAST where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Error (strMsg)
import Control.Applicative ((<|>))

import Language.BLang.Data
import Language.BLang.Error
import Language.BLang.Miscellaneous
import qualified Language.BLang.FrontEnd.Parser as P
import Language.BLang.Semantic.Type

-- desugar user-defined type
tyDesugar :: MonadWriter [CompileError] m => P.AST -> m P.AST
tyDesugar ast = liftM fst $ runReaderT (runStateT (mapM tyDeMTop ast) emptyA) emptyA

tyDeMTop :: (MonadReader (Assoc String P.Type) m, MonadState (Assoc String P.Type) m, MonadWriter [CompileError] m)
         => P.ASTTop -> m P.ASTTop
tyDeMTop (P.VarDeclList decls) = liftM P.VarDeclList $ tyDeMDecls decls
tyDeMTop (P.FuncDecl ls ty name args code) = do
  ty' <- deTy (head ls) ty
  args' <- zipWithM (mapsnd . deTy) (tail (tail ls)) args
  code' <- tyDeMStmt code
  return (P.FuncDecl ls ty' name args' code')

tyDeMDecls :: (MonadReader (Assoc String P.Type) m, MonadState (Assoc String P.Type) m, MonadWriter [CompileError] m)
           => [P.ASTDecl] -> m [P.ASTDecl]
tyDeMDecls decls = do
  (ls, vs) <- liftM unzip $ tyDeMDecls' decls
  return [P.VarDecl ls vs]

tyDeMDecls' :: (MonadReader (Assoc String P.Type) m, MonadState (Assoc String P.Type) m, MonadWriter [CompileError] m)
            => [P.ASTDecl] -> m [(Line, (String, P.Type, Maybe P.ASTStmt))]
tyDeMDecls' ((P.VarDecl ls decls):rest) = do
  decls' <- zipWithM (map2nd . deTy) ls decls
  rest' <- tyDeMDecls' rest
  return (zip ls decls' ++ rest')
tyDeMDecls' ((P.TypeDecl ls decls):rest) = insertTys ls decls >> tyDeMDecls' rest
tyDeMDecls' [] = return []

tyDeMStmt :: (MonadReader (Assoc String P.Type) m, MonadState (Assoc String P.Type) m, MonadWriter [CompileError] m)
          => P.ASTStmt -> m P.ASTStmt
tyDeMStmt (P.Block decls stmts) = do
  (decls', stmts') <- runLocal $ do
    decls' <- tyDeMDecls decls
    stmts' <- mapM tyDeMStmt stmts
    return (decls', stmts')
  return $ P.Block decls' stmts'
tyDeMStmt for@(P.For _ _ _ _ code) = do
  code' <- tyDeMStmt code
  return for{ P.forCode = code' }
tyDeMStmt while@(P.While _ _ code) = do
  code' <- tyDeMStmt code
  return while{ P.whileCode = code' }
tyDeMStmt (P.If line con th el) = do
  th' <- tyDeMStmt th
  el' <- maybeM el tyDeMStmt
  return (P.If line con th' el')
tyDeMStmt s = return s -- Expr, Ap, Return, Identifier, LiteralVal, ArrayRef, Nop

insertTys :: (MonadReader (Assoc String P.Type) m, MonadState (Assoc String P.Type) m, MonadWriter [CompileError] m)
          => [Line] -> [(String, P.Type)] -> m ()
insertTys ls tys = do
  tys' <- zipWithM (mapsnd . deTy) ls tys -- recursive definition is not allowed
  zipWithM_ insertTy ls tys'

insertTy :: (MonadReader (Assoc String P.Type) m, MonadState (Assoc String P.Type) m, MonadWriter [CompileError] m)
         => Line -> (String, P.Type) -> m ()
insertTy line (name, ty) = do
  currScope <- get
  when (name `memberA` currScope) $
    tell [errorAt line "type name redeclared"] -- TODO: line number
  put (insertA name ty currScope)

deTy :: (MonadReader (Assoc String P.Type) m, MonadState (Assoc String P.Type) m, MonadWriter [CompileError] m)
     => Line -> P.Type -> m P.Type
deTy line (P.TPtr t) = liftM P.TPtr (deTy line t)
deTy line (P.TArray ixs t) = do
  t' <- deTy line t
  case t' of -- merge array types
    (P.TArray ixs' t'') -> return $ P.TArray (ixs ++ ixs') t''
    _ -> return $ P.TArray ixs t'
deTy line (P.TCustom name) = do
  currScope <- get
  upperScope <- ask
  case lookupA name currScope <|> lookupA name upperScope of
    Just ty -> return ty
    Nothing -> do
      tell [errorAt line $ "variable has unknown base type '" ++ name ++ "'"]
      return (P.TCustom name)
deTy _ t = return t -- TInt, TFloat, TVoid, TChar

-- desugar function array type
fnArrDesugar :: P.AST -> P.AST
fnArrDesugar = map fnArrDeTop

fnArrDeTop :: P.ASTTop -> P.ASTTop
fnArrDeTop f@(P.FuncDecl _ _ _ args _) = f{ P.funcArgs = map toPtr args }
fnArrDeTop decl = decl

toPtr :: (String, P.Type) -> (String, P.Type)
toPtr (name, ty) = (name, tyParserArrayDecay ty)
