{-# LANGUAGE FlexibleContexts, DoRec #-}

-- module for transforming semantic IR into an ANF-inspired IR
module Language.BLang.BackEnd.LLIRTrans where

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
import qualified Language.BLang.BackEnd.LLIR as L

-- global state
data St = St { getRegCnt :: Int -- next available register number
             , getBlockCnt :: Int -- next available block number
             , getRegTypes :: Assoc L.Reg L.Type -- the type of each register
             , getLocalVars :: Assoc String L.Type -- (current) function's local variables
             , getCurrBlock :: L.Label
             , getCodes :: Assoc L.Label [L.AST] } -- existed blocks

updateRegCnt    f st = st { getRegCnt = f (getRegCnt st) }
updateBlockCnt  f st = st { getBlockCnt = f (getBlockCnt st) }
updateRegTypes  f st = st { getRegTypes = f (getRegTypes st) }
setLocalVars vars st = st { getLocalVars = vars }
updateCodes     f st = st { getCodes = f (getCodes st) }
setCurrBlock  lbl st = st { getCurrBlock = lbl }

shortCircuitVar = "short_circuit_tmp"

freshReg :: (MonadState St m, Functor m)
         => L.Type -> m L.Reg
freshReg ty = do
  n <- getRegCnt <$> get
  modify $ updateRegCnt (+1)
  let reg = L.TempReg n
  modify $ updateRegTypes (insertA reg ty)
  return reg

freshLabel :: (MonadState St m, Functor m) => m L.Label
freshLabel = modify (updateBlockCnt (+1)) >> L.BlockLabel . (subtract 1) . getBlockCnt <$> get

runNewControl :: (MonadState St m, Functor m)
             => m [L.AST] -> m L.Label -- label for entrance
runNewControl codeGen = do
  currLabel <- getCurrBlock <$> get
  lbl <- freshLabel
  modify $ setCurrBlock lbl
  codes <- codeGen
  modify $ updateCodes $ insertA lbl codes
  modify $ setCurrBlock currLabel
  return lbl

llirTrans :: S.Prog S.Type -> L.Prog L.VarInfo
llirTrans (S.Prog decls funcs) = L.Prog decls' funcs' regs'
  where decls' = mapWithKeyA L.VarInfo . filterA notFunc $ decls
        progs' = runState (T.mapM id $ mapWithKeyA (llTransFunc decls) funcs) initState
        funcs' = fst progs'
        regs'  = getRegTypes . snd $ progs'
        initState = St 0 0 emptyA emptyA (error "not in a block") emptyA
        notFunc (S.TArrow _ _) = False
        notFunc _              = True

llTransFunc :: (MonadState St m, MonadFix m, Applicative m)
            => Assoc String S.Type -> String -> S.FuncDecl S.Type -> m (L.Func L.VarInfo)
llTransFunc globalEnv name (S.FuncDecl retTy args vars code) = do
  modify $ updateCodes (const emptyA)
  modify $ setCurrBlock (error "not in a block")
  modify $ setLocalVars (vars `unionA` globalEnv)
  let retVal L.TVoid = Nothing
      retVal L.TInt = Just $ L.Constant (L.IntLiteral 0)
      retVal L.TFloat = Just $ L.Constant (L.FloatLiteral 0.0)
  entryLbl <- runNewControl (llTransAST code [L.Return (retVal retTy)])
  codes <- getCodes <$> get
  let vars' = mapWithKeyA L.VarInfo $ insertA shortCircuitVar L.TInt vars
  return $ L.Func name args vars' entryLbl codes

-- translate S.AST into LLIR AST. -- MonadIO for testing
llTransAST :: (MonadState St m, MonadFix m, Applicative m)
           => [S.AST S.Type] -> [L.AST] -> m [L.AST]
llTransAST ((S.For forinit forcond foriter forcode):cs) k =
  llTransAST forinit =<<
  llTransAST [S.While forcond' (forcode ++ foriter)] =<<
  llTransAST cs k
  where forcond' = if null forcond then [S.LiteralVal (S.IntLiteral 1)] else forcond
llTransAST ((S.While whcond whcode):cs) k = do
  rec
    whCondIn <- runNewControl $
      cpsExpr (last whcond) $ KBool
        (return [L.Jump whCodeIn])
        (return [L.Jump finalBlockIn])
        (\val -> loadVal val $ \reg -> return [L.Branch reg whCodeIn finalBlockIn])

    whCodeIn <- runNewControl (llTransAST whcode [L.Jump whCondIn])

    finalBlockIn <- runNewControl (llTransAST cs k)
  return [L.Jump whCondIn]
llTransAST ((S.If con th el):cs) k = do
  rec
    let elseBlockIn = case el of
          Nothing -> finalBlockIn
          _ -> maybeElseBlockIn

    conCode <- cpsExpr con $ KBool
      (return [L.Jump thenBlockIn])
      (return [L.Jump elseBlockIn])
      (\val -> loadVal val $ \reg -> return [L.Branch reg thenBlockIn elseBlockIn])

    thenBlockIn <- runNewControl (llTransAST th [L.Jump finalBlockIn])

    ~(Just maybeElseBlockIn) <- maybeM el $ \el' -> 
      runNewControl (llTransAST el' [L.Jump finalBlockIn])

    finalBlockIn <- runNewControl (llTransAST cs k)
  return conCode
llTransAST ((S.Expr ty S.Assign [rand1, rand2]):cs) k =
  cpsVarRef rand1 rvalError $ \lref ->
  cpsExpr rand2 $ KFn $ \val2 ->
  loadVal val2 $ \reg2 ->
  ((L.Store lref reg2):) <$> llTransAST cs k
  where rvalError = KFn $ \rval -> error $ "Unexpected r-value " ++ show rval ++ " in expression '" 
                                   ++ show (S.Expr ty S.Assign [rand1, rand2]) ++ "'"
llTransAST (s@(S.Ap ty fn args):cs) k =
  cpsExpr s $ KFn $ \val ->
  llTransAST cs k
llTransAST ((S.Return Nothing):cs) _ =
  return [L.Return Nothing]
llTransAST ((S.Return (Just val)):cs) _ =
  cpsExpr val $ KFn $ \val' -> return [L.Return (Just val')]
llTransAST s@(_:_) k =
  error $ "not handled pattern " ++ show s
llTransAST [] k =
  return k

shortCircuitOps :: Assoc S.Operator (L.Value, [a] -> [a])
shortCircuitOps = fromListA
  [(S.LAnd, (L.Constant (L.IntLiteral 0), \[x,y] -> [y,x])),
   (S.LOr,  (L.Constant (L.IntLiteral 1), \[x,y] -> [x,y]))]

getValueType :: (MonadState St m, Functor m)
             => L.Value -> m L.Type
getValueType (L.Constant (L.IntLiteral _)) = return L.TInt
getValueType (L.Constant (L.FloatLiteral _)) = return L.TFloat
getValueType (L.Constant (L.StringLiteral _)) = return (L.TPtr L.TChar)
getValueType (L.Var name) = do
  vars <- getLocalVars <$> get
  return $ case vars!name of
    t@(L.TArray _ _) -> tyArrayDecay t
    t -> L.TPtr t
getValueType (L.Reg reg) = do
  regTy <- getRegTypes <$> get
  return $ regTy!reg

loadVal :: (MonadState St m, Applicative m)
        => L.Value -> (L.Reg -> m [L.AST]) -> m [L.AST]
loadVal (L.Reg srcReg) k = k srcReg
loadVal val k = do -- casting from non-reg: load it to a reg
  valReg <- freshReg =<< getValueType val
  ((L.Val valReg val):) <$> k valReg

data KCont m = KFn (L.Value -> m [L.AST])
             | KBool (m [L.AST]) (m [L.AST]) (L.Value -> m [L.AST])

kAp :: KCont m -> L.Value -> m [L.AST]
kAp (KFn k) v = k v
kAp (KBool _ kf _) (L.Constant (L.IntLiteral 0)) = kf
kAp (KBool _ kf _) (L.Constant (L.FloatLiteral 0.0)) = kf
kAp (KBool kt _ _) (L.Constant _) = kt
kAp (KBool kt _ _) (L.Var _) = kt
kAp (KBool  _ _ k) v = k v 

-- variant of continuation passing style, transforming pure expressions
-- short circuit value is saved to *the* local variable `short_circuit_tmp`
cpsExpr :: (MonadState St m, MonadFix m, Applicative m)
        => S.AST S.Type -> KCont m -> m [L.AST]
cpsExpr (S.Expr ty rator [rand1, rand2]) k@(KBool _ _ _) | rator `memberA` shortCircuitOps = do
  let (circuitVal, putRand1Rand2) = shortCircuitOps!rator
  rec
    let [trueBranch, falseBranch] = putRand1Rand2 [circuitBlockIn, rand2BlockIn]
        [trueCode, falseCode] = putRand1Rand2 [kAp k circuitVal, return [L.Jump rand2BlockIn]]

    code1 <- cpsExpr rand1 $ KBool trueCode falseCode
      (\val1 -> loadVal val1 $ \reg1 -> return [L.Branch reg1 trueBranch falseBranch])

    rand2BlockIn <- runNewControl (cpsExpr rand2 k)

    circuitBlockIn <- runNewControl (kAp k circuitVal)
  return code1
cpsExpr (S.Expr ty rator [rand1, rand2]) k | rator `memberA` shortCircuitOps = do
  let (circuitVal, putRand1Rand2) = shortCircuitOps!rator
  rec
    let [trueBranch, falseBranch] = putRand1Rand2 [finalBlockIn, rand2BlockIn]
        [trueCode, falseCode] = putRand1Rand2
          [ do reg <- freshReg L.TInt
               return [L.Val reg circuitVal,
                       L.Store (Left shortCircuitVar) reg,
                       L.Jump finalBlockIn]
          , return [L.Jump rand2BlockIn]]

    rand1Reg <- freshReg L.TInt
    code1 <- cpsExpr rand1 $ KBool trueCode falseCode
      (\val1 -> do -- k(unknown)
        rand1Reg <- freshReg L.TInt
        return [L.Let rand1Reg L.SetNZ [val1],
                L.Store (Left shortCircuitVar) rand1Reg,
                L.Branch rand1Reg trueBranch falseBranch])

    rand2BlockIn <- runNewControl $ do
      rand2Reg <- freshReg L.TInt
      let cont = [L.Store (Left shortCircuitVar) rand2Reg, L.Jump finalBlockIn]        
      cpsExpr rand2 $ KBool
        (return $ L.Val rand2Reg (L.Constant (L.IntLiteral 1)):cont)
        (return $ L.Val rand2Reg (L.Constant (L.IntLiteral 0)):cont)
        (\val2 -> return $ L.Let rand2Reg L.SetNZ [val2]:cont)

    finalBlockIn <- runNewControl $ do
      dstReg <- freshReg L.TInt
      ((L.Load dstReg (Left shortCircuitVar)):) <$> (kAp k (L.Reg dstReg))
  return code1
cpsExpr (S.Expr ty rator rands) k | rator /= S.Assign = do -- left-to-right evaluation
  dstReg <- freshReg ty
  runContT (mapM (ContT . (. KFn) . cpsExpr) rands) $ \vals ->
    ((L.Let dstReg (L.fromParserOp rator) vals):) <$> kAp k (L.Reg dstReg)
cpsExpr (S.ImplicitCast ty' ty e) k = do
  dstReg <- freshReg ty'
  cpsExpr e $ KFn $ \var ->
    loadVal var $ \srcReg ->
    ((L.Cast dstReg ty' srcReg ty):) <$> kAp k (L.Reg dstReg)
cpsExpr (S.Ap ty (S.Identifier _ fn) args) k = do
  dstReg <- freshReg ty
  runContT (mapM (ContT . (. KFn) . cpsExpr) args) $ \vals ->
    ((L.Call dstReg fn vals):) <$> kAp k (L.Reg dstReg)
cpsExpr (S.LiteralVal lit) k =
  kAp k (L.Constant lit)
cpsExpr s@(S.Identifier _ _) k =
  cpsVarRef s k $ \var -> do
    dstReg <- freshReg =<< getRValType var
    ((L.Load dstReg var):) <$> kAp k (L.Reg dstReg)
cpsExpr s@(S.ArrayRef _ _ _) k =
  cpsVarRef s k $ \var -> do
    dstReg <- freshReg =<< getRValType var
    ((L.Load dstReg var):) <$> kAp k (L.Reg dstReg)
cpsExpr s _ = error $ "Applying `cpse` to non-expression '" ++ show s ++ "'"

getRValType :: (MonadState St m, Functor m) => Either String L.Reg -> m L.Type
getRValType (Left var) = do
  vars <- getLocalVars <$> get
  return (vars!var)
getRValType (Right reg) = do
  regTy <- getRegTypes <$> get
  let L.TPtr ty = regTy!reg
  return ty

cpsVarRef :: (MonadState St m, MonadFix m, Applicative m)
          => S.AST S.Type
          -> KCont m
          -> (Either String L.Reg -> m [L.AST])
          -> m [L.AST]
cpsVarRef (S.Identifier ty name) k contLoadRVal = do -- array decay here
  vars <- getLocalVars <$> get
  case vars!name of
    L.TArray _ _ -> kAp k (L.Var name)
    _ -> contLoadRVal (Left name)
cpsVarRef (S.ArrayRef ty ref idx) k contLoadRVal = do
  vars <- getLocalVars <$> get
  getBaseRef vars ref $ \baseRef ->
    cpsExpr idx $ KFn $ \idxVal -> do
      -- we did *not* dereference the *last* dimension, only evaluating it to
      -- an l-value. Of course, for the inner refence, ``dereference" do happen
      let ty' = case ty of { L.TPtr _ -> ty; otherwise -> L.TPtr ty }
      dstReg <- freshReg ty'
      ((L.ArrayRef dstReg baseRef idxVal siz):) <$> derefArr ty (L.Reg dstReg)
  where
    getBaseRef vars (S.Identifier _ name) k' =
      case vars!name of
        t@(L.TPtr _) -> do
          dstReg <- freshReg t
          ((L.Load dstReg (Left name)):) <$> k' (Right dstReg)
        L.TArray _ _ -> k' (Left name)
        otherwise -> error $ "getBaseRef: unknown type " ++ name ++ ":" ++ show (vars!name)
    getBaseRef _ _ k' = cpsVarRef ref (KFn $ \(L.Reg reg) -> k' (Right reg)) contLoadRVal
    S.TPtr ty' = S.getType ref
    siz = tySize ty'
    derefArr (S.TPtr _) val = kAp k val
    derefArr _ (L.Reg srcReg) = contLoadRVal (Right srcReg)
