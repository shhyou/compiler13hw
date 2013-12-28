{-# LANGUAGE FlexibleContexts, DoRec #-}

-- module for transforming semantic IR into an ANF-inspired IR
module Language.BLang.CodeGen.LLIRTrans where

import Control.Applicative (Applicative(), (<$>), (<*>))
import Control.Monad (forM)
import Control.Monad.Fix
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Cont

import Language.BLang.Data

import qualified Language.BLang.Semantic.AST as S
import Language.BLang.Semantic.Type
import qualified Language.BLang.CodeGen.LLIR as L

import Debug.Trace

-- global state
data St = St { getRegCnt :: Int -- next available register number
             , getBlockCnt :: Int -- next available block number
             , getCurrBlock :: L.Label -- current
             , getExitBlock :: L.Label
             , getCodes :: Assoc L.Label [L.AST] } -- existed blocks

updateRegCnt   f st = st { getRegCnt = f (getRegCnt st) }
updateBlockCnt f st = st { getBlockCnt = f (getBlockCnt st) }
updateCodes    f st = st { getCodes = f (getCodes st) }
setExitBlock lbl st = st { getExitBlock = lbl }
setCurrBlock lbl st = st { getCurrBlock = lbl }

freshReg :: (MonadState St m, Functor m) => m L.Reg
freshReg = modify (updateRegCnt (+1)) >> L.TempReg . (subtract 1) . getRegCnt <$> get

freshLabel :: (MonadState St m, Functor m) => m L.Label
freshLabel = modify (updateBlockCnt (+1)) >> L.BlockLabel . (subtract 1) . getBlockCnt <$> get

-- runs the program `m` in a new block, returning the lable of the exit block
-- `m` is assumed to have exactly one exit
runNewBlock :: (MonadIO m, MonadState St m, Functor m)
            => ((m [L.AST] -> m [L.AST]) -> m [L.AST]) -> m (L.Label, L.Label)
runNewBlock m = do
  currBlock <- getCurrBlock <$> get
  lbl <- freshLabel
  modify $ setCurrBlock lbl
  codes <- m $ \m' -> do
    exitLbl <- getCurrBlock <$> get
    liftIO $ putStrLn $ "got exitLbl in: " ++ show exitLbl
    code <- m'
    modify $ setExitBlock exitLbl
    return code
  exitLbl <- getExitBlock <$> get
  liftIO $ putStrLn $ "got exitLbl:" ++ show exitLbl
  modify $ updateCodes (insertA lbl codes)
  -- liftIO $ putStrLn $ "resetting block back to " ++ show currBlock
  modify $ setCurrBlock currBlock
  return (lbl, exitLbl)

llirTrans :: S.Prog S.Var -> L.Prog L.VarInfo
llirTrans (S.Prog decls funcs) = undefined
{-
-- translate S.AST into LLIR AST.
llTransAST :: (MonadReader (Assoc String S.Var) m, MonadState St m, MonadFix m, Applicative m)
           => [S.AST S.Var] -> [L.AST] -> m [L.AST]
llTransAST ((S.Block sym codes):cs) k = local (sym `unionA`) $ do
  undefined
llTransAST ((S.For _ forinit forcond foriter forcode):cs) k = undefined
llTransAST ((S.While _ whcond whcode):cs) k = undefined
llTransAST ((S.If _ con th Nothing):cs) k = undefined
llTransAST ((S.If _ con th (Just el)):cs) k = undefined
llTransAST ((S.Expr _ _ S.Assign [rand1, rand2]):cs) k = undefined
llTransAST ((S.Return _ Nothing):cs) _ =
  return [L.Return Nothing]
llTransAST ((S.Return _ (Just val)):cs) _ =
  cpsExpr val $ \val' -> return [L.Return (Just val')]
llTransAST (S.Nop:cs) k =
  llTransAST cs k
-}
shortCircuitOps :: Assoc S.Operator (L.Value, [a] -> [a])
shortCircuitOps = fromListA
  [(S.LAnd, (L.Constant (L.IntLiteral 0), \[x,y] -> [y,x])),
   (S.LOr,  (L.Constant (L.IntLiteral 1), \[x,y] -> [x,y]))]

loadVal :: (MonadState St m, Applicative m)
        => L.Value -> (L.Reg -> m [L.AST]) -> m [L.AST]
loadVal (L.Reg srcReg) k = k srcReg
loadVal val k = do -- casting from non-reg: load it to a reg
  valReg <- freshReg
  ((L.Val valReg val):) <$> k valReg

-- variant of continuation passing style, transforming pure expressions
cpsExpr :: (MonadIO m, MonadState St m, MonadFix m, Applicative m)
        => S.AST S.Var -> (L.Value -> m [L.AST]) -> m [L.AST]
cpsExpr (S.Expr ty _ rator [rand1, rand2]) k | rator `memberA` shortCircuitOps = do
  let (shortCircuitVal, xchg) = shortCircuitOps ! rator
  rec
    let phi = xchg [(rand1Block, shortCircuitVal), (rand2ExitBlock, L.Reg tmpReg)]
    (finalBlock, _) <- runNewBlock $ \runExitBlock -> do
      dstReg <- freshReg
      liftIO $ putStrLn $ "final block of " ++ show rator
      runExitBlock $
        ((L.Phi dstReg phi):) <$> k (L.Reg dstReg)

    tmpReg <- freshReg
    (rand2Block, rand2ExitBlock) <- runNewBlock $ \runExitBlock ->
      cpsExpr rand2 $ \val2 ->
      runExitBlock $ do 
      blk <- getCurrBlock <$> get
      liftIO $ putStrLn $ "evaluating " ++ show rand2 ++ " in block " ++ show blk
      return [L.Let tmpReg L.SetNZ [val2],
              L.Jump finalBlock]
    liftIO $ putStrLn $ "rand2ExitBlock " ++ show rand2ExitBlock ++ ", " ++ show rand2

    let [trueBlock, falseBlock] = xchg [finalBlock, rand2Block]
    rand1BlockCode <- cpsExpr rand1 $ \val1 ->
      loadVal val1 $ \reg1 ->
      return [L.Branch reg1 trueBlock falseBlock]
    rand1Block <- getCurrBlock <$> get
    liftIO $ putStrLn $ "in block " ++ show rand1Block ++ ", " ++ show rator
  return rand1BlockCode

cpsExpr (S.Expr ty _ rator rands) k | rator /= S.Assign = do -- left-to-right evaluation
  dstReg <- freshReg
  runContT (mapM (ContT . cpsExpr) rands) $ \vals ->
    ((L.Let dstReg (L.fromParserOp rator) vals):) <$> k (L.Reg dstReg)
cpsExpr (S.ImplicitCast ty' ty e) k = do
  dstReg <- freshReg
  cpsExpr e $ \var ->
    loadVal var $ \srcReg ->
    ((L.Cast dstReg ty' srcReg ty):) <$> k (L.Reg dstReg)
cpsExpr (S.Ap ty _ (S.Identifier _ _ fn) args) k = do
  dstReg <- freshReg
  runContT (mapM (ContT . cpsExpr) args) $ \vals ->
    ((L.Call dstReg fn vals):) <$> k (L.Reg dstReg)
cpsExpr (S.LiteralVal _ lit) k =
  k (L.Constant lit)
cpsExpr s@(S.Identifier _ _ _) k =
  cpsVarRef s k $ \var -> do
    dstReg <- freshReg
    ((L.Load dstReg var):) <$> k (L.Reg dstReg)
cpsExpr s@(S.ArrayRef _ _ _ _) k =
  cpsVarRef s k $ \var -> do
    dstReg <- freshReg
    ((L.Load dstReg var):) <$> k (L.Reg dstReg)
cpsExpr s _ = error $ "Applying `cpse` to non-expression '" ++ show s ++ "'"

cpsVarRef :: (MonadIO m, MonadState St m, MonadFix m, Applicative m)
          => S.AST S.Var
          -> (L.Value -> m [L.AST])
          -> (Either String L.Reg -> m [L.AST])
          -> m [L.AST]
cpsVarRef (S.Identifier ty _ name) k contLRVal =
  contLRVal (Left name)
cpsVarRef (S.ArrayRef ty _ ref idx) k contLRVal =
  getBaseRef ref $ \baseRef ->
  cpsExpr idx $ \idxVal -> do
    dstReg <- freshReg
    ((L.ArrayRef dstReg baseRef idxVal siz):) <$> derefArr ty (L.Reg dstReg)
  where
    getBaseRef (S.Identifier _ _ name) k' = k' (Left name)
    getBaseRef _ k' = cpsVarRef ref (\(L.Reg reg) -> k' (Right reg)) contLRVal
    S.TPtr ty' = S.getType ref
    siz = tySize ty'
    derefArr (S.TPtr _) val = k val
    derefArr _ (L.Reg srcReg) = contLRVal (Right srcReg)

-- eliminate `phi` functions, if MIPSTrans module doesn't support `phi`.
phiElim :: ()
phiElim = undefined
