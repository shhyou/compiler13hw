{-# LANGUAGE FlexibleContexts #-}

module Language.BLang.Semantic.SymTable where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Error (strMsg)
import Control.Applicative ((<|>))

import Language.BLang.Data
import Language.BLang.Error
import Language.BLang.Miscellaneous
import Language.BLang.Semantic.Type
import qualified Language.BLang.FrontEnd.Parser as P
import qualified Language.BLang.Semantic.AST as S

data Var = Var { varType :: S.Type, varInit :: Maybe (S.AST Var) }

data GlobalDecl = GlobalDecl { varDecl :: Assoc String Var, funcDecl :: Assoc String (S.FuncDecl Var) }

buildSymTable :: MonadWriter [CompileError] m => P.AST -> m (S.Prog Var)
buildSymTable ast = do
  (_, GlobalDecl vardecl funcdecl) <- runStateT (mapM_ buildMTop ast) (GlobalDecl emptyA emptyA)
  -- insert built-in functions
  let vardecl0 = insertA "read"  (Var (S.TArrow [] S.TInt) Nothing) vardecl
      vardecl1 = insertA "fread" (Var (S.TArrow [] S.TFloat) Nothing) vardecl0
      vardecl2 = insertA "write" (Var (S.TArrow [S.TPtr S.TVoid] S.TVoid) Nothing) vardecl1
  return $ S.Prog vardecl2 funcdecl

setVarDecl :: (Assoc String Var -> Assoc String Var) -> GlobalDecl -> GlobalDecl
setVarDecl f st = st { varDecl = f . varDecl $ st }

setFuncDecl :: (Assoc String (S.FuncDecl Var) -> Assoc String (S.FuncDecl Var)) -> GlobalDecl -> GlobalDecl
setFuncDecl f st = st { funcDecl = f . funcDecl $ st }

buildMTop :: (MonadState GlobalDecl m, MonadWriter [CompileError] m)
          => P.ASTTop -> m ()
buildMTop (P.VarDeclList [P.VarDecl decls]) = do
  let tyDecls = map (second3 fromParserType) decls
  (_, decls') <- runTop (mapM_ insertSym tyDecls)
  modify $ setVarDecl (const decls')
buildMTop (P.FuncDecl ty name args (P.Block [P.VarDecl decls] stmts)) = do
  let ty' = fromParserType ty
      args' = map (second fromParserType) args
      args3' = map (\(name, ty) -> (name, ty, Nothing)) args'
      decls' = map (second3 fromParserType) decls
  modify $ setVarDecl (insertA name $ Var (S.TArrow (map snd args') ty') Nothing)
  (code', _) <- runTop $ runLocal $ do
    mapM_ insertSym args3'
    mapM_ insertSym decls'
    liftM2 S.Block get (buildMStmts stmts)
  modify $ setFuncDecl $ insertA name $ S.FuncDecl ty' args' code'

buildMStmts :: (MonadReader (Assoc String Var) m, MonadState (Assoc String Var) m, MonadWriter [CompileError] m)
            => [P.ASTStmt] -> m [S.AST Var]
buildMStmts = mapM buildMStmt . filter (/= P.Nop)

buildMStmt :: (MonadReader (Assoc String Var) m, MonadState (Assoc String Var) m, MonadWriter [CompileError] m)
           => P.ASTStmt -> m (S.AST Var)
buildMStmt (P.Block [P.VarDecl decls] stmts) = do
  (symtable, stmts') <- runLocal $ do
    mapM_ insertSym (map (second3 fromParserType) decls)
    stmts' <- buildMStmts stmts
    symtable <- get
    return (symtable, stmts')
  return $ S.Block symtable stmts'
buildMStmt (P.Expr op stmt) = return . S.Expr undefined op =<< buildMStmts stmt
buildMStmt (P.For forinit forcond foriter forcode) = do
  forinit' <- buildMStmts forinit
  forcond' <- buildMStmts forcond
  foriter' <- buildMStmts foriter
  forcode' <- runLocal (buildMStmt forcode)
  return $ S.For forinit' forcond' foriter' forcode'
buildMStmt (P.While whcond whcode) = do
  whcond' <- buildMStmts whcond
  whcode' <- runLocal (buildMStmt whcode)
  return $ S.While whcond' whcode'
buildMStmt (P.Ap fn args) = do
  fn' <- buildMStmt fn
  args' <- buildMStmts args
  return $ S.Ap undefined fn' args'
buildMStmt (P.If con th el) = do
  con' <- buildMStmt con
  th' <- runLocal (buildMStmt th)
  el' <- maybeM el (runLocal . buildMStmt)
  return $ S.If con' th' el'
buildMStmt (P.Return val) = liftM S.Return (maybeM val buildMStmt)
buildMStmt (P.Identifier name) = do
  currScope <- get
  upperScope <- ask
  when ((not $ name `memberA` currScope) && (not $ name `memberA` upperScope)) $
    tell [strMsg "Undeclared identifier"] -- TODO: line number
  return $ S.Identifier undefined name
buildMStmt (P.LiteralVal lit) = return $ S.LiteralVal lit
buildMStmt (P.ArrayRef exp ix) = liftM2 (S.Deref undefined) (buildMStmt exp) (buildMStmt ix)
buildMStmt P.Nop = fail "Should not get P.Nop"

runTop :: (MonadState GlobalDecl m, MonadWriter [CompileError] m)
       => StateT (Assoc String Var) (ReaderT (Assoc String Var) m) a -> m (a, Assoc String Var)
runTop m = liftM varDecl get >>= \varEnv -> runReaderT (runStateT m varEnv) emptyA

-- note: In `int a = a + 1`, the latter `a` refers to the newly declared `a`
insertSym :: (MonadReader (Assoc String Var) m, MonadState (Assoc String Var) m, MonadWriter [CompileError] m)
          => (String, S.Type, Maybe P.ASTStmt) -> m ()
insertSym (name, ty, varinit) = do
  currScope <- get
  when (name `memberA` currScope) $
    tell [strMsg "Identifier redeclared"] -- TODO: add line number
  put (insertA name (Var ty Nothing) currScope) -- Hence, put the declaration anyway
  maybeM varinit $ \initexpr -> do
    varinit' <- buildMStmt initexpr -- shouldn't be modifying symtbl
    put (insertA name (Var ty (Just varinit')) currScope)
  return ()