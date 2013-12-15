{-# LANGUAGE FlexibleContexts #-}

module Language.BLang.Semantic.SymTable where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Applicative ((<|>))

import Language.BLang.Data
import Language.BLang.Error
import Language.BLang.Miscellaneous
import Language.BLang.Semantic.Type
import qualified Language.BLang.FrontEnd.Parser as P
import qualified Language.BLang.Semantic.AST as S

data Var = Var { varType :: S.Type, varInit :: Maybe (S.AST Var) }

buildSymTable :: MonadWriter [CompileError] m => P.AST -> m (S.Prog Var)
buildSymTable = undefined

data GlobalDecl = GlobalDecl { varDecl :: Assoc String Var, funcDecl :: Assoc String (S.FuncDecl Var) }

setVarDecl :: (Assoc String Var -> Assoc String Var) -> GlobalDecl -> GlobalDecl
setVarDecl f st = st { varDecl = f . varDecl $ st }

setFuncDecl :: (Assoc String (S.FuncDecl Var) -> Assoc String (S.FuncDecl Var)) -> GlobalDecl -> GlobalDecl
setFuncDecl f st = st { funcDecl = f . funcDecl $ st }

buildMTop :: (MonadState GlobalDecl m, MonadWriter [CompileError] m)
          => P.ASTTop -> m ()
buildMTop (P.VarDeclList [P.VarDecl decls]) = do
  let tyDecls = map (\(name, ty, varinit) -> (name, fromParserType ty, varinit)) decls
  (_, decls') <- runReaderT (runStateT (mapM insertSym tyDecls) undefined) emptyA -- TODO: current declared func
  modify $ setVarDecl (decls' ++) -- TODO: insert list
buildMTop (P.FuncDecl ty name args code) = undefined

buildMStmt :: (MonadReader (Assoc String Var) m, MonadState (Assoc String Var) m, MonadWriter [CompileError] m)
           => P.ASTStmt -> m (S.AST Var)
buildMStmt = undefined

insertSym :: (MonadReader (Assoc String Var) m, MonadState (Assoc String Var) m, MonadWriter [CompileError] m)
          => (String, S.Type, Maybe P.ASTStmt) -> m ()
insertSym = undefined
