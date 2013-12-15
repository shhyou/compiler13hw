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
import qualified Language.BLang.FrontEnd.Parser as P

-- evaluate values in array type declarations. a[1+2] => a[3]
constFolding :: MonadWriter [CompileError] m => P.AST -> m P.AST
constFolding = mapM mapMTop

mapMTop :: MonadWriter [CompileError] m => P.ASTTop -> m P.ASTTop
mapMTop (P.VarDeclList decls) = liftM P.VarDeclList $ mapM mapMDecl decls
mapMTop f@(P.FuncDecl _ _ args code) = do
  args' <- mapM mapSnd args
  code' <- mapMStmt code
  return (f{ P.funcArgs = args', P.funcCode = code' })

mapMDecl :: MonadWriter [CompileError] m => P.ASTDecl -> m P.ASTDecl
mapMDecl (P.TypeDecl decls) = liftM P.TypeDecl $ mapM mapSnd decls
mapMDecl (P.VarDecl  decls) = liftM P.VarDecl $ mapM map2nd decls

mapMStmt :: MonadWriter [CompileError] m => P.ASTStmt -> m P.ASTStmt
mapMStmt (P.Block decls stmts) = liftM2 P.Block (mapM mapMDecl decls) (mapM mapMStmt stmts)
mapMStmt for@(P.For _ _ _ code) = mapMStmt code >>= \code' -> return (for{ P.forCode = code' })
mapMStmt while@(P.While _ code) = mapMStmt code >>= \code' -> return (while{ P.whileCode = code' })
mapMStmt (P.If con th Nothing) = liftM2 (P.If con) (mapMStmt th) (return Nothing)
mapMStmt (P.If con th (Just el)) = do
  th' <- mapMStmt th
  el' <- mapMStmt el
  return $ P.If con th' (Just el')
mapMStmt s = return s -- Expr, Ap, Return Identifier, LiteralVal, ArrayRef, Nop

mapSnd :: MonadWriter [CompileError] m => (a, P.Type) -> m (a, P.Type)
mapSnd (a, t) = liftM2 (,) (return a) (foldType t)

map2nd :: MonadWriter [CompileError] m => (a, P.Type, c) -> m (a, P.Type, c)
map2nd (a, t, c) = liftM3 (,,) (return a) (foldType t) (return c)

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
evalIx (P.LiteralVal (P.IntLiteral n)) = return n
evalIx (P.Expr rator [rand])
  | Just f <- lookup rator op1Code = liftM f (evalIx rand)
evalIx (P.Expr rator [rand1, rand2])
  | Just g <- lookup rator op2Code = liftM2 g (evalIx rand1) (evalIx rand2)
evalIx (P.Identifier _) = fail "array dimension can only have constant expressions" -- TODO: line number
evalIx _ = fail "array dimension declaration should be of integer type" -- TODO: line number

foldType :: MonadWriter [CompileError] m => P.Type -> m P.Type
foldType (P.TArray ixs t) = do
  ixs' <- case runIdentity $ runErrorT $ mapM evalIx ixs of
            Left ce -> tell [ce] >> return ixs
            Right ixs' -> return . map P.LiteralVal . map P.IntLiteral $ ixs'
  return (P.TArray ixs' t)
foldType (P.TPtr t) = liftM P.TPtr (foldType t)
foldType t = return t
