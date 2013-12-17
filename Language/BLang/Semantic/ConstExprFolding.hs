{-# LANGUAGE FlexibleContexts #-}

module Language.BLang.Semantic.ConstExprFolding (
  constFolding
) where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.Error

import Language.BLang.Data
import Language.BLang.Error
import Language.BLang.Miscellaneous
import qualified Language.BLang.FrontEnd.Parser as P

-- evaluate values in array type declarations. a[1+2] => a[3]
constFolding :: MonadWriter [CompileError] m => P.AST -> m P.AST
constFolding = mapM mapMTop

mapMTop :: MonadWriter [CompileError] m => P.ASTTop -> m P.ASTTop
mapMTop (P.VarDeclList decls) = liftM P.VarDeclList $ mapM mapMDecl decls
mapMTop f@(P.FuncDecl (_:_:ls) _ _ args code) = do
  args' <- zipWithM (mapsnd . foldType) ls args
  code' <- mapMStmt code
  return (f{ P.funcArgs = args', P.funcCode = code' })

mapMDecl :: MonadWriter [CompileError] m => P.ASTDecl -> m P.ASTDecl
mapMDecl (P.TypeDecl ls decls) = liftM (P.TypeDecl ls) $ zipWithM (mapsnd . foldType) ls decls
mapMDecl (P.VarDecl  ls decls) = liftM (P.VarDecl ls) $ zipWithM (map2nd . foldType) ls decls

mapMStmt :: MonadWriter [CompileError] m => P.ASTStmt -> m P.ASTStmt
mapMStmt (P.Block decls stmts) = liftM2 P.Block (mapM mapMDecl decls) (mapM mapMStmt stmts)
mapMStmt for@(P.For _ _ _ _ code) = mapMStmt code >>= \code' -> return (for{ P.forCode = code' })
mapMStmt while@(P.While _ _ code) = mapMStmt code >>= \code' -> return (while{ P.whileCode = code' })
mapMStmt (P.If pos con th el) = do
  th' <- mapMStmt th
  el' <- maybeM el mapMStmt
  return $ P.If pos con th' el'
mapMStmt s = return s -- Expr, Ap, Return Identifier, LiteralVal, ArrayRef, Nop

toBool :: Integer -> Bool
toBool 0 = False
toBool _ = True

fromBool :: Bool -> Integer
fromBool False = 0
fromBool True = 1

fromLogic :: (Bool -> Bool -> Bool) -> Integer -> Integer -> Integer
fromLogic f = \x y -> fromBool $ f (toBool x) (toBool y)

fromCmp :: (Integer -> Integer -> Bool) -> Integer -> Integer -> Integer
fromCmp = ((fromBool .) .)

op1Code :: [(P.Operator, Integer -> Integer)]
op1Code = [(P.Negate, (0-)), (P.LNot, fromBool . not . toBool)]

op2Code :: [(P.Operator, Integer -> Integer -> Integer)]
op2Code = [(P.Plus, (+)), (P.Minus, (-)), (P.Times, (*)), (P.Divide, div),
           (P.LT, fromCmp (<)), (P.GT, fromCmp (>)), (P.LEQ, fromCmp (<=)), (P.GEQ, fromCmp (>=)), (P.EQ, fromCmp (==)), (P.NEQ, fromCmp (/=)),
           (P.LOr, fromLogic (||)), (P.LAnd, fromLogic (&&))]

evalIx :: MonadError CompileError m => P.ASTStmt -> m Integer
evalIx (P.LiteralVal _ (P.IntLiteral n)) = return n
evalIx (P.Expr _ rator [rand])
  | Just f <- lookup rator op1Code = liftM f (evalIx rand)
evalIx (P.Expr _ rator [rand1, rand2])
  | Just g <- lookup rator op2Code = liftM2 g (evalIx rand1) (evalIx rand2)
evalIx (P.Identifier line _) = throwError $ errorAt line "array dimension can only have constant expressions" -- TODO: line number
evalIx stmt = throwError $ errorAt line "array dimension declaration should be of integer type" -- TODO: line number
  where line = case P.getStmtLine stmt of
          Just lin -> lin
          Nothing  -> NoLineInfo
foldType :: MonadWriter [CompileError] m => Line -> P.Type -> m P.Type
foldType line (P.TArray ixs t) = do
  let checkPositive n = when (n <= 0) $ throwError $ errorAt line "array should be of positive dimension"
      reduceIxs = do
        ixs' <- mapM evalIx ixs
        mapM_ checkPositive ixs'
        return ixs'
  ixs' <- case runIdentity $ runErrorT $ reduceIxs of
            Left ce -> tell [ce] >> return ixs
            Right ixs' -> return . map (P.LiteralVal line) . map P.IntLiteral $ ixs'
  return (P.TArray ixs' t)
foldType line (P.TPtr t) = liftM P.TPtr (foldType line t)
foldType _ t = return t
