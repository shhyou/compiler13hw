{-# LANGUAGE FlexibleContexts #-}

module Language.BLang.Semantic.TypeCheck where

import Data.List (intercalate)
import qualified Data.Traversable as T
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Error (strMsg)

import Language.BLang.Data
import Language.BLang.Error
import Language.BLang.Miscellaneous
import qualified Language.BLang.Semantic.AST as S
import Language.BLang.Semantic.SymTable
import Language.BLang.Semantic.Type

typeCheck :: MonadWriter [CompileError] m => S.Prog Var -> m (S.Prog Var)
typeCheck (S.Prog vardecls fundecls) = do
  vardecls' <- T.forM vardecls $ \(Var ty line varinit) -> do
    case varinit of
      Just expr | tyIsArithType ty -> do
        expr' <- runReaderT (tyCheckAST expr) (TypeEnv vardecls (error "No global environment"))
        return $ Var ty line (Just expr')
      Nothing -> return $ Var ty line Nothing
      _ -> do
        tell [errorAt line $ "Initializing variable from incompatible type"]
        return $ Var ty line varinit
  fundecls' <- T.forM fundecls $ \fn -> do
    code' <- runReaderT (tyCheckAST $ S.funcCode fn) (TypeEnv vardecls fn)
    return $ fn { S.funcCode = code' }
  return $ S.Prog vardecls' fundecls'

data TypeEnv = TypeEnv { typeDecls :: Assoc String Var,
                         currFunc :: S.FuncDecl Var }

setTypeDecls :: Assoc String Var -> TypeEnv -> TypeEnv
setTypeDecls symtbl env = env { typeDecls = symtbl }

-- Reader for visible bindings and current function, encapsulated in `TypeEnv`
tyCheckAST :: (MonadReader TypeEnv m, MonadWriter [CompileError] m)
         => S.AST Var -> m (S.AST Var)
tyCheckAST (S.Block symtbl stmts) = -- TODO: check inits
  local (setTypeDecls symtbl) $ do
    symtbl' <- T.forM symtbl $ \(Var ty line varinit) -> do
      case varinit of
        Just expr -> do
          expr' <- tyCheckAST expr
          let ty' = S.getType expr'
          when (not $ tyIsStrictlyCompatibleType ty ty') $
            tell [errorAt line $ "Initializing variable from incompatible type"]
          return $ Var ty line (Just expr')
        Nothing -> return $ Var ty line Nothing
    stmts' <- mapM tyCheckAST stmts
    return $ S.Block symtbl' stmts'
tyCheckAST (S.For line forinit forcond foriter forcode) = do
  forinit' <- mapM tyCheckAST forinit
  forcond' <- mapM tyCheckAST forcond
  foriter' <- mapM tyCheckAST foriter
  forcode' <- tyCheckAST forcode
  let t = S.getType (last forcond')
  when ((not $ null $ forcond') && (not $ tyIsScalarType t)) $
    tell [errorAt line $ "Expecting scalar type but got '" ++ show t ++ "'"]
  return $ S.For line forinit' forcond' foriter' forcode'
tyCheckAST (S.While line whcond whcode) = do
  whcond' <- mapM tyCheckAST whcond -- `whcond` /= [] by the grammar
  whcode' <- tyCheckAST whcode
  let t = S.getType (last whcond')
  when (not $ tyIsScalarType t) $
    tell [errorAt line $ "Expecting scalar type but got '" ++ show t ++ "'"]
  return $ S.While line whcond' whcode'
tyCheckAST (S.If line con th el) = do
  con' <- tyCheckAST con
  th' <- tyCheckAST th
  el' <- maybeM el tyCheckAST
  let t = S.getType con'
  when (not $ tyIsScalarType t) $
    tell [errorAt line $ "Expecting scalar type but got '" ++ show t ++ "'"]
  return $ S.If line con' th' el'
tyCheckAST (S.Return line val) = do -- n1570 6.8.6.4
  val' <- maybeM val tyCheckAST
  tyRet <- liftM (S.returnType . currFunc) ask
  let failed ty = tell [errorAt line $ "Cannot match '" ++ show ty ++ "' with expected return type '" ++ show tyRet ++ "'"]
  val'' <- case val' of
    Just valRet -> do
      let t = S.getType valRet
      when (not $ tyIsStrictlyCompatibleType tyRet t) (failed t)
      return $ Just $ tyTypeConv tyRet t valRet
    Nothing -> do
      when (tyRet /= S.TVoid) (failed S.TVoid)
      return Nothing
  return $ S.Return line val''
tyCheckAST (S.Expr _ line S.Negate [rand]) = do
  rand' <- tyCheckAST rand
  let t = S.getType rand'
      t' = tyIntPromotion t
  when (not $ tyIsIntType t) $ tell [errorAt line "'negate' can only take arithmetic type operand"]
  return $ tyTypeConv t' t (S.Expr t line S.Negate [rand'])
tyCheckAST (S.Expr _ line S.LNot [rand]) = do
  rand' <- tyCheckAST rand
  let t = S.getType rand'
  when (not $ tyIsScalarType t) $ tell [errorAt line "'not' can only take scalar type operand"]
  return $ S.Expr S.TInt line S.LNot [rand']
tyCheckAST (S.Expr _ line rator [rand1, rand2])
  | rator `elem` arithOps = do
  rand1' <- tyCheckAST rand1
  rand2' <- tyCheckAST rand2
  let (t1, t2) = (S.getType rand1', S.getType rand2')
      t = tyUsualArithConv t1 t2 -- bottom when failed; non-strictness is important
  if ((not $ tyIsArithType t1) || (not $ tyIsArithType t2))
    then do
      tell [errorAt line $ "'" ++ show rator ++ "' can only take arithmetic type operands"]
      return $ S.Expr S.TVoid line rator [rand1', rand2']
    else do
      return $ S.Expr t line rator [tyTypeConv t t1 rand1', tyTypeConv t t2 rand2']
tyCheckAST (S.Expr _ line rator [rand1, rand2])
  | rator `elem` relOps = do
  rand1' <- tyCheckAST rand1
  rand2' <- tyCheckAST rand2
  case (S.getType rand1', S.getType rand2') of
    (t1, t2) | tyIsArithType t1 && tyIsArithType t2 -> do
      let t = tyUsualArithConv t1 t2
      return $ S.Expr t line rator [tyTypeConv t t1 rand1', tyTypeConv t t2 rand2']
    (t1, t2) | t1 /= S.TVoid && t1 == t2 ->
      return $ S.Expr t1 line rator [rand1', rand2']
    otherwise -> do
      tell [errorAt line $ "'" ++ show rator ++ "' is applied to operands of incompatible types"]
      return $ S.Expr S.TVoid line rator [rand1', rand2']
tyCheckAST (S.Expr _ line rator [rand1, rand2])
  | rator `elem` logicOps = do
  rand1' <- tyCheckAST rand1
  rand2' <- tyCheckAST rand2
  let (t1, t2) = (S.getType rand1', S.getType rand2')
  when ((not $ tyIsScalarType t1) || (not $ tyIsScalarType t2)) $
    tell [errorAt line $ "'" ++ show rator ++ "' is applied to operands of non-scalar type"]
  return $ S.Expr S.TInt line rator [rand1', rand2']
tyCheckAST (S.Expr _ line S.Assign [rand1, rand2]) = do
  rand1' <- tyCheckAST rand1 -- rand1 is will be an lvalue by the grammar if it is not an array
  rand2' <- tyCheckAST rand2
  let (t1, t2) = (S.getType rand1', S.getType rand2')
  when ((not $ tyIsArithType t1) || (not $ tyIsArithType t2)) $
    tell [errorAt line $ "'Assign' is applied to operands of incompatible types or non-lvalues"]
  return $ S.Expr t1 line S.Assign [rand1', tyTypeConv t1 t2 rand2']
tyCheckAST (S.Ap _ apline fn args) = do -- n1570 6.5.2.2
  fn'@(S.Identifier _ line name) <- tyCheckAST fn
  args' <- mapM tyCheckAST args
  let tyArgs' = map S.getType args'
      failed ty = return $ S.Ap ty apline fn' args'
  case S.getType fn' of
    S.TArrow tyArgs tyRet
      | length tyArgs' /= length tyArgs -> do
        tell [errorAt line $ "Cannot unify '" ++ showProdType tyArgs ++ "' with expected type '"
              ++ showProdType tyArgs' ++ "' in the function call to '" ++ name ++ "':\n"
              ++ "| Incorrect number of arguments."]
        failed tyRet
      | or $ zipWith ((not .) . tyIsStrictlyCompatibleType) tyArgs tyArgs' -> do
        let badArgs = tyIncompatibleArgs 1 tyArgs tyArgs'
        tell [errorAt line $ "Cannot unify '" ++ showProdType tyArgs' ++ "' with expected type '"
              ++ showProdType tyArgs ++ "' in the function call to '" ++ name ++ "':\n"
              ++ intercalate "\n" (map ("| " ++) badArgs)]
        failed tyRet
      | otherwise -> do
        return $ S.Ap tyRet apline fn' $ zipWith ($) (zipWith tyTypeConv tyArgs tyArgs') args'
    tyFn -> do
      tell [errorAt line $ "Cannot unify '" ++ showProdType tyArgs' ++ " -> T' with expected type '"
            ++ show tyFn ++ "' in the function call to '" ++ name ++ "'"]
      failed S.TVoid
tyCheckAST (S.Identifier _ line name) = do
  vars <- liftM typeDecls ask
  ty' <- case lookupA name vars of
    Just (Var ty _ _) -> return (tyArrayDecay ty)
    Nothing -> return S.TVoid
  return $ S.Identifier ty' line name
tyCheckAST s@(S.LiteralVal _ lit) = return s
tyCheckAST (S.Deref _ line ref idx) = do -- n1570 6.5.2.1
  ref' <- tyCheckAST ref
  idx' <- tyCheckAST idx
  case (S.getType ref', S.getType idx') of
    (S.TPtr ty, tyIx) | tyIsIntType tyIx ->
      return $ S.Deref (tyArrayDecay ty) line ref' (tyTypeConv S.TInt tyIx idx')
    (S.TPtr ty, _) -> do
      tell [errorAt line $ "Array subscript should be of integer type"]
      return $ S.Deref (tyArrayDecay ty) line ref' idx'
    (ty, _) -> do
      tell [errorAt line $ "Array subscripting error: expecting pointer (array) type but got '" ++ show ty ++ "'"]
      return $ S.Deref S.TVoid line ref' idx'
tyCheckAST S.Nop = return S.Nop

showProdType :: [S.Type] -> String
showProdType ts = "(" ++ intercalate ", " (map show ts) ++ ")"

tyIncompatibleArgs :: Int -> [S.Type] -> [S.Type] -> [String]
tyIncompatibleArgs n (t:ts) (t':ts')
  | not (tyIsStrictlyCompatibleType t t') =
    ("In argument " ++ show n ++ ": expecting '" ++ show t ++ "' but got '" ++ show t' ++ "'")
    : tyIncompatibleArgs (n+1) ts ts'
tyIncompatibleArgs n (_:ts) (_:ts') = tyIncompatibleArgs (n+1) ts ts'
tyIncompatibleArgs _ _ _ = []

-- insert implicit type conversion
--           new type  old type
tyTypeConv :: S.Type -> S.Type -> S.AST Var -> S.AST Var
tyTypeConv t' t = if t' == t then id else S.ImplicitCast t' t

arithOps, relOps, logicOps :: [S.Operator]
arithOps = [S.Plus, S.Minus, S.Times, S.Divide]
relOps   = [S.LT, S.GT, S.LEQ, S.GEQ, S.EQ, S.NEQ] -- n1570 6.5.8, 6.5.9, simplified
logicOps = [S.LOr, S.LAnd]
