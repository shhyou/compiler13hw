{-# LANGUAGE FlexibleContexts #-}

module Language.BLang.Semantic.TypeCheck (
  typeCheck
) where

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
import Language.BLang.Semantic.Type
import qualified Language.BLang.Semantic.RawAST as S

data TypeEnv = TypeEnv { typeDecls :: Assoc String S.Var,
                         currFunc :: S.FuncDecl S.Var }

typeCheck :: MonadWriter [CompileError] m => S.Prog S.Var -> m (S.Prog S.Var)
typeCheck (S.Prog vardecls fundecls) = do
  let topEnv = TypeEnv (fromListA vardecls) (error "No global environment")
  vardecls' <- forM vardecls $ \(name, S.Var ty line varinit) -> do
    case varinit of
      Just expr | tyIsArithType ty -> do
        expr' <- runReaderT (tyCheckAST expr) topEnv
        return (name, S.Var ty line (Just expr'))
      Nothing -> return (name, S.Var ty line Nothing)
      _ -> do
        tell [errorAt line $ "Initializing variable from incompatible type"]
        return (name, S.Var ty line varinit)
  fundecls' <- T.forM fundecls $ \fn -> do
    code' <- runReaderT (tyCheckAST $ S.funcCode fn) topEnv{ currFunc = fn }
    return $ fn { S.funcCode = code' }
  return $ S.Prog vardecls' fundecls'

modifyTypeDecls :: (Assoc String S.Var -> Assoc String S.Var) -> TypeEnv -> TypeEnv
modifyTypeDecls updateSymtbl env = env { typeDecls = updateSymtbl $ typeDecls env }

-- Reader for visible bindings and current function, encapsulated in `TypeEnv`
tyCheckAST :: (MonadReader TypeEnv m, MonadWriter [CompileError] m)
         => S.AST S.Var -> m (S.AST S.Var)
tyCheckAST (S.Block symtbl stmts) = do
  currEnv <- liftM typeDecls ask
  let _:envs = scanl (\tbl (name, var) -> insertA name var tbl) currEnv symtbl
      env'   = if null symtbl then currEnv else last envs
  vars <- forM (zip envs symtbl) $ \(env, (name, S.Var ty line varinit)) -> do
    local (modifyTypeDecls (const env)) $ do
      case varinit of
        Just expr -> do
          expr' <- tyCheckAST expr
          let ty' = S.getType expr'
          when (not $ tyIsStrictlyCompatibleType ty ty') $
            tell [errorAt line $ "Initializing variable '" ++ name ++ "' of type '"
                  ++ show ty ++ "' from incompatible type '" ++ show ty' ++ "'"]
          return $ S.Var ty line (Just expr')
        Nothing -> return $ S.Var ty line Nothing
  let symtbl' = zip (map fst symtbl) vars
  stmts' <- local (modifyTypeDecls (const env')) (mapM tyCheckAST stmts)
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
      return $ S.Expr S.TInt line rator [tyTypeConv t t1 rand1', tyTypeConv t t2 rand2']
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
    tell [errorAt line $ "'Assign' is applied to operands of incompatible type or non-lvalues"]
  return $ S.Expr t1 line S.Assign [rand1', tyTypeConv t1 t2 rand2']
tyCheckAST (S.Ap _ apline fn args) = do -- n1570 6.5.2.2
  fn'@(S.Identifier _ line name) <- tyCheckAST fn
  args' <- mapM tyCheckAST args
  let tyArgs' = map S.getType args'
      failed ty = return $ S.Ap ty apline fn' args'
  case S.getType fn' of
    S.TArrow tyArgs tyRet
      | length tyArgs' /= length tyArgs -> do
        tell [errorAt line $ "Cannot unify '" ++ showProdType tyArgs' ++ "' with expected type '"
              ++ showProdType tyArgs ++ "' in the function call to '" ++ name ++ "':\n"
              ++ "| Incorrect number of arguments."]
        failed tyRet
      | [S.TPtr S.TVoid] <- tyArgs, S.TVoid <- tyRet,
        name == "write" -> -- overloaded `write`. Users wouldn't be able to declare S.TPtr S.TVoid;
        let writeFunc name' ty = S.Identifier (S.TArrow [ty] S.TVoid) NoLineInfo name' in
        case tyArgs' of    -- it must be the built-in function `write`.
          [S.TInt] ->         return $ S.Ap tyRet apline (writeFunc "write" S.TInt) args'
          [S.TFloat] ->       return $ S.Ap tyRet apline (writeFunc "fwrite" S.TFloat) args'
          [S.TPtr S.TChar] -> return $ S.Ap tyRet apline (writeFunc "swrite" $ S.TPtr S.TChar) args'
          otherwise -> do
            tell [errorAt line $ "no match function for call to 'write : " ++ showProdType tyArgs'
                  ++ " -> T':\n| candidates are:\n"
                  ++ "|   write : " ++ showProdType [S.TInt] ++ " -> " ++ show S.TVoid ++ "\n"
                  ++ "|   write : " ++ showProdType [S.TFloat] ++ " -> " ++ show S.TVoid ++ "\n"
                  ++ "|   write : " ++ showProdType [S.TPtr S.TChar] ++ " -> " ++ show S.TVoid ++ "\n"]
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
      tell [errorAt line $ "Cannot unify '" ++ show tyFn ++ "'"
            ++ "with expected type '" ++ showProdType tyArgs' ++ " -> T' "
            ++ "in the function call to '" ++ name ++ "'"]
      failed S.TVoid
tyCheckAST (S.Identifier _ line name) = do
  vars <- liftM typeDecls ask
  ty' <- case lookupA name vars of
    Just (S.Var ty _ _) -> return (tyArrayDecay ty)
    Nothing -> return S.TVoid
  return $ S.Identifier ty' line name
tyCheckAST s@(S.LiteralVal _ lit) = return s
tyCheckAST (S.ArrayRef _ line ref idx) = do -- n1570 6.5.2.1
  ref' <- tyCheckAST ref
  idx' <- tyCheckAST idx
  case (S.getType ref', S.getType idx') of
    (S.TPtr ty, tyIx) | tyIsIntType tyIx ->
      return $ S.ArrayRef (tyArrayDecay ty) line ref' (tyTypeConv S.TInt tyIx idx')
    (S.TPtr ty, _) -> do
      tell [errorAt line $ "Array subscript should be of integer type"]
      return $ S.ArrayRef (tyArrayDecay ty) line ref' idx'
    (ty, _) -> do
      tell [errorAt line $ "Array subscripting error: expecting pointer (array) type but got '" ++ show ty ++ "'"]
      return $ S.ArrayRef S.TVoid line ref' idx'
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
tyTypeConv :: S.Type -> S.Type -> S.AST S.Var -> S.AST S.Var
tyTypeConv t' t = if t' == t then id else S.ImplicitCast t' t

arithOps, relOps, logicOps :: [S.Operator]
arithOps = [S.Plus, S.Minus, S.Times, S.Divide]
relOps   = [S.LT, S.GT, S.LEQ, S.GEQ, S.EQ, S.NEQ] -- n1570 6.5.8, 6.5.9, simplified
logicOps = [S.LOr, S.LAnd]
