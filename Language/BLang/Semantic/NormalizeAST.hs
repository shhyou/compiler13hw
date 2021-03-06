{-# LANGUAGE FlexibleContexts #-}

module Language.BLang.Semantic.NormalizeAST where

import qualified Data.Traversable as T (forM)
import Control.Applicative (Applicative(), (<$>), (<*>))
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader

import Language.BLang.Data
import Language.BLang.Miscellaneous
import qualified Language.BLang.Semantic.AST as N
import qualified Language.BLang.Semantic.RawAST as S

normalize :: S.Prog S.Var -> N.Prog S.Type
normalize = removeVarInit . alphaConv

-- alpha conversion, so that no shadowing would occur
-- 'main' function should remain unchanged
data NameSt = NameSt { getFreshSymCnt :: Int,
                       getNameMapping :: Assoc String String }

updateFreshSymCnt f st = st { getFreshSymCnt = f (getFreshSymCnt st) }
updateNameMapping f st = st { getNameMapping = f (getNameMapping st) }

alphaConv :: S.Prog S.Var -> S.Prog S.Var
alphaConv (S.Prog decls funcs) =
  let funcs' = mapKeysA alphaFuncName funcs in
  fst $ runIdentity $
  runStateT (S.Prog <$> alphaConvVars decls <*> T.forM funcs' alphaConvFunc)
            (NameSt 0 emptyA)

alphaFuncName :: String -> String
alphaFuncName "main" = "main"
alphaFuncName "read" = "read"
alphaFuncName "sread" = "sread"
alphaFuncName "fread" = "fread"
alphaFuncName "write" = "write"
alphaFuncName "swrite" = "swrite"
alphaFuncName "fwrite" = "fwrite"
alphaFuncName fn     = "_fn_" ++ fn

alphaConvFunc :: (MonadState NameSt m, Applicative m)
               => S.FuncDecl S.Var -> m (S.FuncDecl S.Var)
alphaConvFunc (S.FuncDecl ty env args code) = runLocal $ do
  currEnv <- getNameMapping <$> get
  let (names, types) = unzip args
      env' = fromListA $ map (first (currEnv !)) $ toListA env
  (code', names') <- runLocal $ do
    code' <- alphaConvBlock' code
    env <- getNameMapping <$> get
    let names' = map (env !) names
    return (code', names')
  return $ S.FuncDecl ty env' (zip names' types) code'

alphaConvVars :: (MonadState NameSt m, Applicative m)
              => [(String, S.Var)] -> m [(String, S.Var)]
alphaConvVars vars = forM vars $ \(name, S.Var ty line varinit) -> do
  name' <- case ty of
    S.TArrow _ _ -> return $ alphaFuncName name
    otherwise    -> freshSym name
  insertMap name name'
  varinit' <- maybeM varinit alphaConvAST
  return (name', S.Var ty line varinit')

alphaConvBlock' :: (MonadState NameSt m, Applicative m)
                => S.AST S.Var -> m (S.AST S.Var)
alphaConvBlock' (S.Block symtbl codes) =
  S.Block <$> alphaConvVars symtbl <*> mapM alphaConvAST codes

alphaConvAST :: (MonadState NameSt m, Applicative m) -- there should be no scoping issue
             => S.AST S.Var -> m (S.AST S.Var)
alphaConvAST s@(S.Block _ _) =
  runLocal $ alphaConvBlock' s
alphaConvAST (S.Expr ty line rand rators) =
  S.Expr ty line rand <$> mapM alphaConvAST rators
alphaConvAST (S.ImplicitCast ty' ty e) =
  S.ImplicitCast ty' ty <$> alphaConvAST e
alphaConvAST (S.For line forinit forcond foriter forcode) =
  S.For line <$> mapM alphaConvAST forinit <*> mapM alphaConvAST forcond <*> mapM alphaConvAST foriter <*> alphaConvAST forcode
alphaConvAST (S.While line whcond whcode) =
  S.While line <$> mapM alphaConvAST whcond <*> alphaConvAST whcode
alphaConvAST (S.Ap ty line fn args) =
  S.Ap ty line <$> alphaConvAST fn <*> mapM alphaConvAST args
alphaConvAST (S.If line con th el) =
  S.If line <$> alphaConvAST con <*> alphaConvAST th <*> maybeM el alphaConvAST
alphaConvAST (S.Return line val) =
  S.Return line <$> maybeM val alphaConvAST
alphaConvAST (S.ArrayRef ty line ref idx) =
  S.ArrayRef ty line <$> alphaConvAST ref <*> alphaConvAST idx
alphaConvAST (S.Identifier ty line name) =
  S.Identifier ty line <$> ((! name) . getNameMapping <$> get)
alphaConvAST s = -- LiteralVal, Nop
  return s

freshSym :: (MonadState NameSt m, Functor m) => String -> m String
freshSym str = do
  n <- getFreshSymCnt <$> get
  modify (updateFreshSymCnt (+1))
  return $ str ++ "_" ++ show n

runLocal :: (MonadState NameSt m, Functor m) => m a -> m a
runLocal m = do
  symtbl <- getNameMapping <$> get
  a <- m
  modify $ updateNameMapping (const symtbl)
  return a

insertMap name name' = modify $ updateNameMapping (insertA name name')

-- remove global/local variable initialization code and remove 
removeVarInit :: S.Prog S.Var -> N.Prog N.Type
removeVarInit (S.Prog decls funcs) = N.Prog decls' funcs''
  where (decls', varinits) = remVar decls
        funcs' = fmap remVarFunc funcs
        funcs'' = adjustA insertGlobalInits "main" funcs'
        insertGlobalInits fn@(N.FuncDecl _ _ _ _) = fn { N.funcCode = varinits ++ N.funcCode fn }

remVar :: [(String, S.Var)] -> (Assoc String N.Type, [N.AST N.Type])
remVar vars = (vars', inst)
  where vars' = fromListA $ map (second S.varType) vars
        inst = concatMap initInst vars
        initInst (var, S.Var ty _ Nothing) = []
        initInst (var, S.Var ty _ (Just varinit)) = [N.Expr ty N.Assign [N.Identifier ty var, remVarAST varinit]]

remVarFunc :: S.FuncDecl S.Var -> N.FuncDecl N.Type
remVarFunc (S.FuncDecl tyRet _ args code) = N.FuncDecl tyRet args vars code'
  where (code', vars) = runIdentity $ runStateT (remVarStmt code) emptyA

remVarStmt :: (MonadState (Assoc String N.Type) m, Applicative m)
           => S.AST S.Var -> m [N.AST N.Type]
remVarStmt (S.Block symtbl stmts) = do
  modify (symtbl' `unionA`)
  stmts' <- concat <$> mapM remVarStmt stmts
  return (varinits ++ stmts')
  where (symtbl', varinits) = remVar symtbl
remVarStmt (S.For _ forinit forcond foriter forcode) = do
  forcode' <- remVarStmt forcode
  return [N.For (map remVarAST forinit) (map remVarAST forcond) (map remVarAST foriter) forcode']
remVarStmt (S.While _ whcond whcode) = do
  whcode' <- remVarStmt whcode
  return [N.While (map remVarAST whcond) whcode']
remVarStmt (S.If _ con th el) = do
  th' <- remVarStmt th
  el' <- maybeM el remVarStmt
  return [N.If (remVarAST con) th' el']
remVarStmt S.Nop = return []
remVarStmt e = return [remVarAST e]

remVarAST :: S.AST S.Var -> N.AST N.Type
remVarAST (S.Expr ty _ op stmts) =
  N.Expr ty op (map remVarAST stmts)
remVarAST (S.ImplicitCast ty' ty stmt) =
  N.ImplicitCast ty' ty (remVarAST stmt)
remVarAST (S.Ap ty _ fn args) =
  N.Ap ty (remVarAST fn) (map remVarAST args)
remVarAST (S.Return _ val) =
  N.Return (fmap remVarAST val)
remVarAST (S.Identifier ty _ name) =
  N.Identifier ty name
remVarAST (S.LiteralVal _ lit) =
  N.LiteralVal lit
remVarAST (S.ArrayRef ty _ base idx) =
  N.ArrayRef ty (remVarAST base) (remVarAST idx)
