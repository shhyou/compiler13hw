{-# LANGUAGE FlexibleContexts #-}

module Language.BLang.Semantic.SymTable (
  buildSymTable
) where

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
import qualified Language.BLang.Semantic.RawAST as S

data GlobalDecl = GlobalDecl { varDecl :: [(String, S.Var)],
                               funcDecl :: Assoc String (S.FuncDecl S.Var) }

buildSymTable :: MonadWriter [CompileError] m => P.AST -> m (S.Prog S.Var)
buildSymTable ast = do
  let vardecl = [("read",  (S.Var (S.TArrow [] S.TInt) NoLineInfo Nothing)),
                 ("fread", (S.Var (S.TArrow [] S.TFloat) NoLineInfo Nothing)),
                 ("write", (S.Var (S.TArrow [S.TPtr S.TVoid] S.TVoid) NoLineInfo Nothing))]
  (_, GlobalDecl vardecl funcdecl) <- runStateT (mapM_ buildMTop ast) (GlobalDecl vardecl emptyA)
  let vardecl' = reverse $ filter (not . tyIsTypeSynonym . S.varType . snd) vardecl
  case lookup "main" vardecl' of
    Just (S.Var (S.TArrow [] S.TInt) _ _) -> return ()
    Just (S.Var t line _) -> tell [errorAt line $ "expecting 'main' to be 'TArrow [] TInt' but got '" ++ show t ++ "'"]
    Nothing -> tell [strMsg "'main' function not found"]
  -- insert built-in functions
  return $ S.Prog vardecl' funcdecl

setVarDecl :: ([(String, S.Var)] -> [(String, S.Var)]) -> GlobalDecl -> GlobalDecl
setVarDecl f st = st { varDecl = f . varDecl $ st }

setFuncDecl :: (Assoc String (S.FuncDecl S.Var) -> Assoc String (S.FuncDecl S.Var)) -> GlobalDecl -> GlobalDecl
setFuncDecl f st = st { funcDecl = f . funcDecl $ st }

buildMTop :: (MonadState GlobalDecl m, MonadWriter [CompileError] m)
          => P.ASTTop -> m ()
buildMTop (P.VarDeclList [P.VarDecl ls decls]) = do
  let tyDecls = map (second3 fromParserType) decls
  (_, decls') <- runTop (zipWithM_ insertSym ls tyDecls)
  modify $ setVarDecl (const decls')
buildMTop (P.FuncDecl (lty:lname:ls) ty name args code) = do
  let ty' = fromParserType ty
      args' = map (second fromParserType) args
      args3' = map (\(name, ty) -> (name, ty, Nothing)) args'
  (_, vardecl') <- runTop $ insertSym lname (name, S.TArrow (map snd args') ty', Nothing)
  modify $ setVarDecl (const vardecl')
  (code', _) <- runTop $ runLocal $ do
    zipWithM_ insertSym ls args3'
    buildMBlock' code -- runs in current scope: parameters are of the same scope
  currEnv <- liftM (fromListA . varDecl) get
  modify $ setFuncDecl $ insertA name $ S.FuncDecl ty' currEnv args' code'

buildMStmts :: (MonadReader [(String, S.Var)] m, MonadState [(String, S.Var)] m, MonadWriter [CompileError] m)
            => [P.ASTStmt] -> m [S.AST S.Var]
buildMStmts = mapM buildMStmt . filter notNop
  where notNop P.Nop = False
        notNop _     = True

buildMStmt :: (MonadReader [(String, S.Var)] m, MonadState [(String, S.Var)] m, MonadWriter [CompileError] m)
           => P.ASTStmt -> m (S.AST S.Var)
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
  let ty = fmap S.varType $ lookup name currScope <|> lookup name upperScope
  case ty of
    Just S.TTypeSyn -> tell [errorAt line $ "Unexpected type synonym '" ++ name ++ "'"]
    Nothing         -> tell [errorAt line $ "Undeclared identifier '" ++ name ++ "'"]
    otherwise       -> return ()
  return $ S.Identifier (error "buildMStmt:Identifier") line name
buildMStmt (P.LiteralVal line lit) = return $ S.LiteralVal line lit
buildMStmt (P.ArrayRef line exp ix) = liftM2 (S.ArrayRef (error "buildMStmt:ArrayRef") line) (buildMStmt exp) (buildMStmt ix)
buildMStmt P.Nop = return S.Nop

-- build a Block in **current scope**. That `{`, `}` creates a new scope should be
-- handled by running `buildMBlock'` using `runLocal`.
buildMBlock' :: (MonadReader [(String, S.Var)] m, MonadState [(String, S.Var)] m, MonadWriter [CompileError] m)
             => P.ASTStmt -> m (S.AST S.Var)
buildMBlock' (P.Block [P.VarDecl ls decls] stmts) = do
  zipWithM_ insertSym ls (map (second3 fromParserType) decls)
  stmts' <- buildMStmts stmts
  symtbl <- liftM (reverse . filter (not . tyIsTypeSynonym . S.varType . snd)) get
  return $ S.Block symtbl stmts'

runTop :: (MonadState GlobalDecl m, MonadWriter [CompileError] m)
       => StateT [(String, S.Var)] (ReaderT [(String, S.Var)] m) a -> m (a, [(String, S.Var)])
runTop m = liftM varDecl get >>= \varEnv -> runReaderT (runStateT m varEnv) []

runLocal :: (MonadReader [(key, val)] m, MonadState [(key, val)] m)
         => m a -> m a
runLocal m = do
  upperState <- ask
  currState <- get
  put []
  a <- local (const $ currState ++ upperState) m
  put currState
  return a

-- note: In `int a = a + 1`, the latter `a` refers to the newly declared `a`
insertSym :: (MonadReader [(String, S.Var)] m, MonadState [(String, S.Var)] m, MonadWriter [CompileError] m)
          => Line -> (String, S.Type, Maybe P.ASTStmt) -> m ()
insertSym line (name, ty, varinit) = do
  currScope <- get
  if (not $ tyIsTypeSynonym ty) && (name `member` currScope)
    then tell [errorAt line $ "Identifier '" ++ name ++ "' redeclared"]
    else put ((name, (S.Var ty line Nothing)):currScope) -- Hence, put the declaration anyway
  maybeM varinit $ \initexpr -> do
    varinit' <- buildMStmt initexpr -- shouldn't be modifying symtbl
    put ((name, (S.Var ty line (Just varinit'))):currScope)
  return ()

member :: Eq a => a -> [(a, b)] -> Bool
member a = (a `elem`) . map fst
