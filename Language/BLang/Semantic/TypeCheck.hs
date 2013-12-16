{-# LANGUAGE FlexibleContexts #-}

module Language.BLang.Semantic.TypeCheck where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Language.BLang.Data
import Language.BLang.Error
import Language.BLang.Miscellaneous
import qualified Language.BLang.Semantic.AST as S
import Language.BLang.Semantic.SymTable
import Language.BLang.Semantic.Type

-- TODO: check variable init, check function code
typeCheck :: MonadWriter [CompileError] m => S.Prog Var -> m (S.Prog Var)
typeCheck = undefined

-- TODO: check ast; Reader for visible bindings, State for current function
tyChkAST :: (MonadReader (Assoc String Var) m, MonadState (S.FuncDecl Var) m, MonadWriter [CompileError] m)
         => S.AST Var -> m (S.AST Var)
tyChkAST (S.Block _ _) = undefined
tyChkAST (S.Expr _ _ _) = undefined
tyChkAST (S.For _ _ _ _) = undefined
tyChkAST (S.While _ _) = undefined
tyChkAST (S.Ap _ _ _) = undefined
tyChkAST (S.If _ _ _) = undefined
tyChkAST (S.Return _) = undefined
tyChkAST (S.Identifier _) = undefined
tyChkAST (S.LiteralVal _) = undefined
tyChkAST (S.Deref _ _ _) = undefined
