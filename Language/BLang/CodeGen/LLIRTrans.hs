{-# LANGUAGE FlexibleContexts, DoRec #-}

-- module for transforming semantic IR into an ANF-inspired IR
module Language.BLang.CodeGen.LLIRTrans where

import qualified Data.Traversable as T (mapM)
import Control.Applicative (Applicative(), (<$>), (<*>))
import Control.Monad (forM)
import Control.Monad.Fix
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Cont

import Language.BLang.Data
import Language.BLang.Miscellaneous

import qualified Language.BLang.Semantic.AST as S
import Language.BLang.Semantic.Type
import qualified Language.BLang.CodeGen.LLIR as L

import Debug.Trace

-- global state
data St = St { getRegCnt :: Int -- next available register number
             , getBlockCnt :: Int -- next available block number
             , getCurrBlock :: L.Label
             , getExitLabel :: Assoc L.Label L.Label
             , getCodes :: Assoc L.Label [L.AST] } -- existed blocks

updateRegCnt    f st = st { getRegCnt = f (getRegCnt st) }
updateBlockCnt  f st = st { getBlockCnt = f (getBlockCnt st) }
updateCodes     f st = st { getCodes = f (getCodes st) }
setCurrBlock  lbl st = st { getCurrBlock = lbl }
updateExitLabel f st = st { getExitLabel = f (getExitLabel st) }

freshReg :: (MonadState St m, Functor m) => m L.Reg
freshReg = modify (updateRegCnt (+1)) >> L.TempReg . (subtract 1) . getRegCnt <$> get

freshLabel :: (MonadState St m, Functor m) => m L.Label
freshLabel = modify (updateBlockCnt (+1)) >> L.BlockLabel . (subtract 1) . getBlockCnt <$> get

runNewControl :: (MonadState St m, Functor m)
             => ((m [L.AST] -> m [L.AST]) -> m [L.AST]) -> m (L.Label, L.Label) -- label for entrance and exit
runNewControl codeGen = do
  currLabel <- getCurrBlock <$> get
  lbl <- freshLabel
  modify $ setCurrBlock lbl
  modify $ updateExitLabel (insertA lbl lbl)
  (codes, exitLbl) <- traceControl codeGen
  modify $ updateCodes $ insertA lbl codes
  modify $ setCurrBlock currLabel
  return (lbl, exitLbl)

traceControl :: (MonadState St m, Functor m)
             => ((m [L.AST] -> m [L.AST]) -> m [L.AST]) -> m ([L.AST], L.Label)
traceControl codeGen = do
  lbl <- getCurrBlock <$> get
  codes <- codeGen $ \m -> do
    lastCode <- m
    exitBlock <- getCurrBlock <$> get
    exitLbl <- (! exitBlock) . getExitLabel <$> get
    modify $ updateExitLabel (adjustA (const exitLbl) lbl)
    return lastCode
  exitLbl <- (! lbl) . getExitLabel <$> get
  return (codes, exitLbl)

llirTrans :: S.Prog S.Type -> IO (L.Prog L.VarInfo)
llirTrans (S.Prog decls funcs) = L.Prog decls' <$> funcs'
  where decls' = mapWithKeyA L.VarInfo . filterA notFunc $ decls
        funcs' = fmap fst $ runStateT (T.mapM id $ mapWithKeyA (llTransFunc decls) funcs) initState
        initState = St 0 0 (error "not in a block") emptyA emptyA
        notFunc (S.TArrow _ _) = False
        notFunc _              = True

llTransFunc :: (MonadIO m, MonadState St m, MonadFix m, Applicative m)
            => Assoc String S.Type -> String -> S.FuncDecl S.Type -> m (L.Func L.VarInfo)
llTransFunc globalEnv name (S.FuncDecl retTy args code) = do
  modify $ updateCodes (const emptyA)
  modify $ setCurrBlock (error "not in a block")
  modify $ updateExitLabel (const emptyA)
  let retVal L.TVoid = Nothing
      retVal L.TInt = Just $ L.Constant (L.IntLiteral 0)
      retVal L.TFloat = Just $ L.Constant (L.FloatLiteral 0.0)
  (entryLbl, exitLbl) <- runNewControl $ \k' ->
    k' $ llTransAST [code] [L.Return (retVal retTy)]
  codes <- getCodes <$> get
  return $ L.Func name args emptyA entryLbl codes
  --                       XXX locals

-- translate S.AST into LLIR AST. -- MonadIO for testing
llTransAST :: (MonadIO m, MonadState St m, MonadFix m, Applicative m)
           => [S.AST S.Type] -> [L.AST] -> m [L.AST]
llTransAST ((S.Block sym codes):cs) k =
  llTransAST codes =<< llTransAST cs k
llTransAST ((S.For forinit forcond foriter forcode):cs) k =
  llTransAST forinit =<<
  llTransAST [S.While forcond' (S.Block symtbl (codes ++ foriter))] =<<
  llTransAST cs k
  where forcond' = if null forcond then [S.LiteralVal (S.IntLiteral 1)] else forcond
        (symtbl, codes) = case forcode of
          S.Block symtbl' codes' -> (symtbl', codes')
          code -> (emptyA, [code])
llTransAST ((S.While whcond whcode):cs) k =
  fmap fst $ traceControl $ \leaveWhileBlock -> do
  rec
    (whCondIn, whCondOut) <- runNewControl $ \leaveBlock ->
      cpsExpr (last whcond) $ \(val, _) ->
      loadVal val $ \reg ->
      leaveBlock $ return [L.Branch reg whCodeIn finalBlockIn]

    (whCodeIn, whCodeOut) <- runNewControl $ \leaveBlock ->
      leaveBlock $ llTransAST [whcode] [L.Jump whCondIn]

    (finalBlockIn, finalBlockOut) <- runNewControl $ \leaveBlock ->
      leaveBlock $ leaveWhileBlock $ llTransAST cs k
  return [L.Jump whCondIn]
llTransAST ((S.If con th el):cs) k =
  fmap fst $ traceControl $ \leaveIfBlock -> do
  rec
    (conCode, codeExitBlock) <- traceControl $ \leaveBlock ->
      cpsExpr con $ \(val, _) ->
      loadVal val $ \reg ->
      leaveBlock $ return [L.Branch reg thenBlockIn elseBlockIn]

    (thenBlockIn, thenBlockOut) <- runNewControl $ \leaveBlock ->
      leaveBlock $ llTransAST [th] [L.Jump finalBlockIn]

    el' <- maybeM el $ \el' -> 
      runNewControl $ \leaveBlock ->
      leaveBlock $ llTransAST [el'] [L.Jump finalBlockIn]
    let ~(Just (maybeElseBlockIn, maybeElseBlockOut)) = el'

    let elseBlockIn = case el of
          Nothing -> finalBlockIn
          _ -> maybeElseBlockIn

    (finalBlockIn, finalBlockOut) <- runNewControl $ \leaveBlock ->
      leaveBlock $ leaveIfBlock $ llTransAST cs k
  return conCode
llTransAST ((S.Expr ty S.Assign [rand1, rand2]):cs) k =
  cpsVarRef rand1 rvalError $ \lref ->
  cpsExpr rand2 $ \(val2, _) ->
  loadVal val2 $ \reg2 ->
  ((L.Store lref reg2):) <$> llTransAST cs k
  where rvalError rval = error $ "Unexpected r-value " ++ show rval++ " in expression '" 
                         ++ show (S.Expr ty S.Assign [rand1, rand2]) ++ "'"
llTransAST (s@(S.Ap ty fn args):cs) k =
  cpsExpr s $ \(val, _) ->
  llTransAST cs k
llTransAST ((S.Return Nothing):cs) _ =
  return [L.Return Nothing]
llTransAST ((S.Return (Just val)):cs) _ =
  cpsExpr val $ \(val', _) -> return [L.Return (Just val')]
llTransAST (S.Nop:cs) k =
  llTransAST cs k
llTransAST s@(_:_) k =
  error $ "not handled pattern " ++ show s
llTransAST [] k =
  return k

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
-- short circuit value is saved to *the* local variable `short_circuit_tmp`
cpsExpr :: (MonadIO m, MonadState St m, MonadFix m, Applicative m)
        => S.AST S.Type -> ((L.Value, Bool) -> m [L.AST]) -> m [L.AST]
cpsExpr (S.Expr ty rator [rand1, rand2]) k | rator `memberA` shortCircuitOps = do
  let (circuitVal, putRand1Rand2) = shortCircuitOps!rator
      shortCircuitVar = "short_circuit_tmp"
  fmap fst $ traceControl $ \exitShortCircuitBlock -> do
    rec
      let [trueBranch, falseBranch] = putRand1Rand2 [finalBlockIn, rand2BlockIn]
      (code1, rand1BlockOut) <- traceControl $ \leaveBlock ->
        cpsExpr rand1 $ \(val1, isShortCircuit) ->
        if isShortCircuit
          then loadVal val1 $ \rand1Reg ->
               leaveBlock $ return [L.Branch rand1Reg trueBranch falseBranch]
          else do
            rand1Reg <- freshReg
            leaveBlock $ return [L.Let rand1Reg L.SetNZ [val1],
                                 L.Store (Left shortCircuitVar) rand1Reg,
                                 L.Branch rand1Reg trueBranch falseBranch]

      rand2Reg <- freshReg
      (rand2BlockIn, rand2BlockOut) <- runNewControl $ \leaveBlock ->
        cpsExpr rand2 $ \(val2, isShortCircuit) ->
        if isShortCircuit
          then leaveBlock $ return [L.Jump finalBlockIn]
          else do
            leaveBlock $ return [L.Let rand2Reg L.SetNZ [val2],
                                 L.Store (Left shortCircuitVar) rand2Reg,
                                 L.Jump finalBlockIn]

      (finalBlockIn, finalBlockOut) <- runNewControl $ \leaveBlock -> do
        dstReg <- freshReg
        leaveBlock $ ((L.Load dstReg (Left shortCircuitVar)):) <$> exitShortCircuitBlock (k (L.Reg dstReg, True))
    return code1
cpsExpr (S.Expr ty rator rands) k | rator /= S.Assign = do -- left-to-right evaluation
  dstReg <- freshReg
  runContT (mapM (ContT . cpsExpr) rands) $ \vals ->
    ((L.Let dstReg (L.fromParserOp rator) (map fst vals)):) <$> k (L.Reg dstReg, False)
cpsExpr (S.ImplicitCast ty' ty e) k = do
  dstReg <- freshReg
  cpsExpr e $ \(var, _) ->
    loadVal var $ \srcReg ->
    ((L.Cast dstReg ty' srcReg ty):) <$> k (L.Reg dstReg, False)
cpsExpr (S.Ap ty (S.Identifier _ fn) args) k = do
  dstReg <- freshReg
  runContT (mapM (ContT . cpsExpr) args) $ \vals ->
    ((L.Call dstReg fn (map fst vals)):) <$> k (L.Reg dstReg, False)
cpsExpr (S.LiteralVal lit) k =
  k (L.Constant lit, False)
cpsExpr s@(S.Identifier _ _) k =
  cpsVarRef s k $ \var -> do
    dstReg <- freshReg
    ((L.Load dstReg var):) <$> k (L.Reg dstReg, False)
cpsExpr s@(S.ArrayRef _ _ _) k =
  cpsVarRef s k $ \var -> do
    dstReg <- freshReg
    ((L.Load dstReg var):) <$> k (L.Reg dstReg, False)
cpsExpr s _ = error $ "Applying `cpse` to non-expression '" ++ show s ++ "'"

cpsVarRef :: (MonadIO m, MonadState St m, MonadFix m, Applicative m)
          => S.AST S.Type
          -> ((L.Value, Bool) -> m [L.AST])
          -> (Either String L.Reg -> m [L.AST])
          -> m [L.AST]
cpsVarRef (S.Identifier ty name) k contLRVal =
  contLRVal (Left name)
cpsVarRef (S.ArrayRef ty ref idx) k contLRVal =
  getBaseRef ref $ \baseRef ->
  cpsExpr idx $ \(idxVal, _) -> do
    dstReg <- freshReg
    ((L.ArrayRef dstReg baseRef idxVal siz):) <$> derefArr ty (L.Reg dstReg)
  where
    getBaseRef (S.Identifier _ name) k' = k' (Left name)
    getBaseRef _ k' = cpsVarRef ref (\(L.Reg reg, _) -> k' (Right reg)) contLRVal
    S.TPtr ty' = S.getType ref
    siz = tySize ty'
    derefArr (S.TPtr _) val = k (val, False)
    derefArr _ (L.Reg srcReg) = contLRVal (Right srcReg)
