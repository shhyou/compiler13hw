{-# LANGUAGE FlexibleContexts #-}

-- module for transforming semantic IR into an ANF-inspired IR
module Language.BLang.CodeGen.LLIRTrans where

import Control.Applicative (Applicative(), (<$>), (<*>))
import Control.Monad.State
import Control.Monad.Reader

import Language.BLang.Data

import qualified Language.BLang.Semantic.AST as S
import qualified Language.BLang.CodeGen.LLIR as L

-- global state
data St = St { getRegCnt :: Int -- next available register number
             , getBlockCnt :: Int -- next available block number
             , getCurrBlock :: Int -- current block number
             , getCodes :: Assoc Int [L.AST] } -- existed blocks

updateRegCnt   f st = st { getRegCnt = f (getRegCnt st) }
updateBlockCnt f st = st { getBlockCnt = f (getBlockCnt st) }
setCurrBlock   n st = st { getCurrBlock = n }
updateCodes    f st = st { getCodes = f (getCodes st) }

freshReg :: (MonadState St m, Functor m) => m Int
freshReg = modify (updateRegCnt (+1)) >> (subtract 1) . getRegCnt <$> get

newBlock :: (MonadState St m, Functor m) => [L.AST] -> m ()
newBlock codes = do
  currBlockNo <- getCurrBlock <$> get
  modify $ updateCodes (insertA currBlockNo codes)
  newBlockNo <- getBlockCnt <$> get
  modify $ updateBlockCnt (+1)
  modify $ setCurrBlock newBlockNo

llirTrans :: S.Prog S.Var -> L.Prog L.VarInfo
llirTrans (S.Prog decls funcs) = undefined

-- translate S.AST into LLIR AST.
llTransAST :: (MonadReader (Assoc String S.Var) m, MonadState St m, Applicative m)
           => [S.AST S.Var] -> [L.AST] -> m [L.AST]
llTransAST ((S.Block sym codes):cs) k = local (sym `unionA`) $ do
  undefined
llTransAST ((S.For _ forinit forcond foriter forcode):cs) k = undefined
llTransAST ((S.While _ whcond whcode):cs) k = undefined
llTransAST ((S.If _ con th Nothing):cs) k = undefined
llTransAST ((S.If _ con th (Just el)):cs) k = undefined
llTransAST ((S.Return _ Nothing):cs) _ =
  return [L.Return Nothing]
llTransAST ((S.Return _ (Just val)):cs) _ =
  cpsExpr val $ \val' -> return [L.Return (Just val')]
llTransAST (S.Nop:cs) k =
  llTransAST cs k

shortCircuitOps :: [S.Operator]
shortCircuitOps = [S.LAnd, S.LOr]

foreachMCont :: (a -> (b -> r) -> r) -> [a] -> ([b] -> r) -> r
foreachMCont _ []     k = k []
foreachMCont f (x:xs) k =
  f x $ \b ->
  foreachMCont f xs $ \bs ->
  k (b:bs)

-- variant of continuation passing style, transforming pure expressions
cpsExpr :: (MonadState St m, Applicative m)
        => S.AST S.Var -> (L.Value -> m [L.AST]) -> m [L.AST]
cpsExpr (S.Expr ty _ rator rands) k | rator `elem` shortCircuitOps =
  undefined
cpsExpr (S.Expr ty _ rator rands) k | rator /= S.Assign = -- left-to-right evaluation
  foreachMCont cpsExpr rands $ \vals -> do
    dstReg <- freshReg
    ((L.Let dstReg rator vals):) <$> k (L.Reg dstReg)
cpsExpr (S.ImplicitCast ty' ty e) k = cpsExpr e castFun
  where castFun (L.Reg srcReg) = do
          dstReg <- freshReg
          ((L.Cast dstReg ty' srcReg ty):) <$> k (L.Reg dstReg)
        castFun val = do -- casting from non-reg: load it to a reg
          tmpReg <- freshReg
          ((L.Val tmpReg val):) <$> castFun (L.Reg tmpReg)
cpsExpr (S.Ap ty _ fn args) k = undefined
cpsExpr (S.Identifier ty _ name) k =
  k (L.Var name)
cpsExpr (S.LiteralVal _ lit) k =
  k (L.Constant lit)
cpsExpr (S.ArrayRef ty _ ref idx) k = undefined
cpsExpr s _ = error $ "Applying `cpse` to non-expression '" ++ show s ++ "'"

-- eliminate `phi` functions, if MIPSTrans module doesn't support `phi`.
phiElim :: ()
phiElim = undefined
