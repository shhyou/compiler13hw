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

data Var = Var { varType :: S.Type, varLine :: Line, varInit :: Maybe (S.AST Var) } deriving (Show)

data GlobalDecl = GlobalDecl { varDecl :: Assoc String Var, funcDecl :: Assoc String (S.FuncDecl Var) }

buildSymTable :: MonadWriter [CompileError] m => P.AST -> m (S.Prog Var)
buildSymTable ast = do
  let vardecl0 = insertA "read"  (Var (S.TArrow [] S.TInt) NoLineInfo Nothing) emptyA
      vardecl1 = insertA "fread" (Var (S.TArrow [] S.TFloat) NoLineInfo Nothing) vardecl0
      vardecl2 = insertA "write" (Var (S.TArrow [S.TPtr S.TVoid] S.TVoid) NoLineInfo Nothing) vardecl1
  (_, GlobalDecl vardecl funcdecl) <- runStateT (mapM_ buildMTop ast) (GlobalDecl vardecl2 emptyA)
  -- insert built-in functions
  return $ S.Prog vardecl funcdecl

setVarDecl :: (Assoc String Var -> Assoc String Var) -> GlobalDecl -> GlobalDecl
setVarDecl f st = st { varDecl = f . varDecl $ st }

setFuncDecl :: (Assoc String (S.FuncDecl Var) -> Assoc String (S.FuncDecl Var)) -> GlobalDecl -> GlobalDecl
setFuncDecl f st = st { funcDecl = f . funcDecl $ st }

buildMTop :: (MonadState GlobalDecl m, MonadWriter [CompileError] m)
          => P.ASTTop -> m ()
buildMTop (P.VarDeclList [P.VarDecl ls decls]) = do
  let tyDecls = map (second3 fromParserType) decls
  (_, decls') <- runTop (zipWithM_ insertSym ls tyDecls)
  modify $ setVarDecl (const decls')
buildMTop (P.FuncDecl ls ty name args code) = do
  let ty' = fromParserType ty
      args' = map (second fromParserType) args
      args3' = map (\(name, ty) -> (name, ty, Nothing)) args'
  (_, vardecl') <- runTop $ insertSym (head (tail ls)) (name, S.TArrow (map snd args') ty', Nothing)
  modify $ setVarDecl (const vardecl')
  (code', _) <- runTop $ runLocal $ do
    zipWithM_ insertSym (tail (tail ls)) args3'
    buildMBlock' code -- runs in current scope: parameters are of the same scope
  modify $ setFuncDecl $ insertA name $ S.FuncDecl ty' args' code'

buildMStmts :: (MonadReader (Assoc String Var) m, MonadState (Assoc String Var) m, MonadWriter [CompileError] m)
            => [P.ASTStmt] -> m [S.AST Var]
buildMStmts = mapM buildMStmt . filter notNop
  where notNop P.Nop = False
        notNop _     = True

buildMStmt :: (MonadReader (Assoc String Var) m, MonadState (Assoc String Var) m, MonadWriter [CompileError] m)
           => P.ASTStmt -> m (S.AST Var)
buildMStmt s@(P.Block _ _) = runLocal $ buildMBlock' s
buildMStmt (P.Expr line op stmt) = return . S.Expr (error "buildMStmt:Expr") line op =<< buildMStmts stmt
buildMStmt (P.For line forinit forcond foriter forcode) = do
  forinit' <- buildMStmts forinit
  forcond' <- buildMStmts forcond
  foriter' <- buildMStmts foriter
  forcode' <- runLocal (buildMStmt forcode)
  return $ S.For line forinit' forcond' foriter' forcode'
buildMStmt (P.While line whcond whcode) = do
  whcond' <- buildMStmts whcond
  whcode' <- runLocal (buildMStmt whcode)
  return $ S.While line whcond' whcode'
buildMStmt (P.Ap line fn args) = do
  fn' <- buildMStmt fn
  args' <- buildMStmts args
  return $ S.Ap (error "buildMStmt:Ap") line fn' args'
buildMStmt (P.If line con th el) = do
  con' <- buildMStmt con
  th' <- runLocal (buildMStmt th)
  el' <- maybeM el (runLocal . buildMStmt)
  return $ S.If line con' th' el'
buildMStmt (P.Return line val) = liftM (S.Return line) (maybeM val buildMStmt)
buildMStmt (P.Identifier line name) = do
  currScope <- get
  upperScope <- ask
  when ((not $ name `memberA` currScope) && (not $ name `memberA` upperScope)) $
    tell [errorAt line $ "Undeclared identifier '" ++ name ++ "'"] -- TODO: line number
  return $ S.Identifier (error "buildMStmt:Identifier") line name
buildMStmt (P.LiteralVal line lit) = return $ S.LiteralVal line lit
buildMStmt (P.ArrayRef line exp ix) = liftM2 (S.Deref (error "buildMStmt:Deref") line) (buildMStmt exp) (buildMStmt ix)
buildMStmt P.Nop = return S.Nop

-- build a Block in **current scope**. That `{`, `}` creates a new scope should be
-- handled by running `buildMBlock'` using `runLocal`.
buildMBlock' :: (MonadReader (Assoc String Var) m, MonadState (Assoc String Var) m, MonadWriter [CompileError] m)
             => P.ASTStmt -> m (S.AST Var)
buildMBlock' (P.Block [P.VarDecl ls decls] stmts) = do
  zipWithM_ insertSym ls (map (second3 fromParserType) decls)
  stmts' <- buildMStmts stmts
  currSymtbl <- get
  upperSymtbl <- ask
  return $ S.Block (currSymtbl `unionA` upperSymtbl) stmts'

runTop :: (MonadState GlobalDecl m, MonadWriter [CompileError] m)
       => StateT (Assoc String Var) (ReaderT (Assoc String Var) m) a -> m (a, Assoc String Var)
runTop m = liftM varDecl get >>= \varEnv -> runReaderT (runStateT m varEnv) emptyA

-- note: In `int a = a + 1`, the latter `a` refers to the newly declared `a`
insertSym :: (MonadReader (Assoc String Var) m, MonadState (Assoc String Var) m, MonadWriter [CompileError] m)
          => Line -> (String, S.Type, Maybe P.ASTStmt) -> m ()
insertSym line (name, ty, varinit) = do
  currScope <- get
  when ((not $ tyIsTypeSynonym ty) && (name `memberA` currScope)) $
    tell [errorAt line $ "Identifier '" ++ name ++ "' redeclared"] -- TODO: add line number
  put (insertA name (Var ty line Nothing) currScope) -- Hence, put the declaration anyway
  maybeM varinit $ \initexpr -> do
    varinit' <- buildMStmt initexpr -- shouldn't be modifying symtbl
    put (insertA name (Var ty line (Just varinit')) currScope)
  return ()
