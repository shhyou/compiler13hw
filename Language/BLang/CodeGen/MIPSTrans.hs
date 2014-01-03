module Language.BLang.CodeGen.MIPSTrans where

import Prelude hiding (div, foldl)
import Data.Foldable (foldlM, foldMap, foldl)
import Data.List (deleteBy)
import Control.Applicative (Applicative(), (<$>), (<*>), pure)
import Control.Monad (zipWithM, mapM, forM)
import Control.Monad.IO.Class

import Debug.Trace

import qualified Language.BLang.Semantic.AST as S
import qualified Language.BLang.CodeGen.LLIR as L
import qualified Language.BLang.CodeGen.AsmIR as A

import Language.BLang.Semantic.Type (tySize)
import Language.BLang.Data


data Obj = OVar String
         | OReg L.Reg
         | OTxt String
         | OInt
         | OFloat
         | OAddr Obj
         deriving (Show, Eq)

instance Ord Obj where
  (OVar va) <= (OVar vb) = va <= vb
  (OReg ra) <= (OReg rb) = ra <= rb
  (OTxt ta) <= (OTxt tb) = ta <= tb
  (OAddr oa) <= (OAddr ob) = oa <= ob
  (OVar _) <= _ = True
  (OReg _) <= _ = True
  (OTxt _) <= _ = True
  OInt <= _ = True
  OFloat <= _ = True
  _ <= _ = False

data Addr = AReg A.Reg
          | AData String
          | AMem Integer A.Reg
          | AVoid
          | AMadoka
          deriving (Show, Eq)

type NameSpace = Assoc Obj (S.Type, Addr)

getOType (x, _) = case x of
  S.TFloat -> OFloat
  _ -> OInt

iregs = map A.SReg [0..7] ++ map A.TReg [0..9]
fregs = filter (/= (A.FReg 12)) $ map A.FReg [0,2..30]
initRegs = iregs ++ fregs
initARegs = map AReg initRegs

isFReg :: A.Reg -> Bool
isFReg (A.FReg _) = True
isFReg _ = False


newtype Foo a = Foo (NameSpace ->
                     [Integer] ->
                     [A.Reg] ->
                     IO (a, [A.Inst], [A.DataVar], NameSpace, [Integer], [A.Reg]))

makeFoo :: [A.Inst] -> [A.DataVar] -> Foo ()
makeFoo y z = Foo $ \w fs q -> return ((), y, z, w, fs, q)

getNS = Foo $ \ns fs q -> return (ns, [], [], ns, fs, q)
editNS f = Foo $ \w fs q -> return ((), [], [], f w, fs, q)

getFrame = Foo $ \ns fs q -> return (fs, [], [], ns, fs, q)
editFrame f = Foo $ \ns fs q -> return ((), [], [], ns, f fs, q)
setFrame = editFrame . const

frameBottom = fmap min getFrame
pushFrame = do
  frame <- getFrame
  let
    oldHeight = minimum frame
    localVarBot = maximum frame
    idx = head $ filter (not . (`elem` frame)) [localVarBot, localVarBot-4..]
  setFrame (idx:frame)
  if idx < oldHeight
    then subi A.SP A.SP (oldHeight - idx)
    else return ()
  return idx

-- does NOT finale the object
popFrame obj = do
  ns <- getNS
  case snd (ns ! obj) of
    AMem idx A.FP -> do
      frame <- getFrame
      let
        oldHeight = minimum frame
        newFrame = filter (/= idx) frame
        newHeight = minimum newFrame
      setFrame newFrame
      if oldHeight < newHeight
        then addi A.SP A.SP (newHeight - oldHeight)
        else return ()
    _ -> error "Object not in frame"

runFoo :: NameSpace -> [Integer] -> [A.Reg] -> Foo a -> IO (a, [A.Inst], [A.DataVar])
runFoo ns fs q (Foo f) = do
  (x, y, z, _, _, _) <- f ns fs q
  return (x, reverse y, reverse z)
runFoo' ns fs q = fmap (\(_, y, z) -> (y, z)) . runFoo ns fs q

instance Functor Foo where
  fmap g (Foo f) = Foo $ \ns fs q -> do (x, y, z, w, s, q') <- f ns fs q
                                        return (g x, y, z, w, s, q')

instance Monad Foo where
  return x = Foo $ \ns fs q -> return (x, [], [], ns, fs, q)
  (Foo f) >>= g = Foo $ \ns fs q -> do
    (x, y, z, ns', fs', q') <- f ns fs q
    let (Foo h) = g x
    (x', y', z', ns'', fs'', q'') <- h ns' fs' q'
    return (x', y' ++ y, z' ++ z, ns'', fs'', q'')

instance MonadIO Foo where
  liftIO io = Foo $ \ns fs q -> io >>= \x -> return (x, [], [], ns, fs, q)

rinst op args = makeFoo [A.RType op args] []
iinst op rd rs imm = makeFoo [A.IType op rd rs imm] []
jinst op imm = makeFoo [A.JType op imm] []
label lbl = makeFoo [A.Label lbl] []

linsts xs = makeFoo xs []


pstring lbl txt = makeFoo [] [(lbl, A.Text txt)]
pword lbl int = makeFoo [] [(lbl, A.Word [int])]
pfloat lbl dbl = makeFoo [] [(lbl, A.Float [dbl])]

addAddr :: Obj -> S.Type -> Addr -> Foo ()
addAddr rd stype addr = editNS $ insertA rd (stype, addr)
setAddr :: Obj -> Addr -> Foo ()
setAddr rd addr = trace ("setAddr " ++ show rd ++ " -> " ++ show addr) $ editNS $ \ns -> insertA rd (fst $ ns ! rd, addr) ns


getQueue = Foo $ \ns fs q -> return (q, [], [], ns, fs, q)
setQueue newQ = Foo $ \ns fs _ -> return ((), [], [], ns, fs, newQ)

enqueue :: A.Reg -> Foo ()
enqueue x = do
  queue <- getQueue
  setQueue $ queue ++ [x]

dequeue x = do
  queue <- getQueue
  setQueue $ filter (/= x) queue
  return x

requeue x = dequeue x >> enqueue x


la rd lbl = iinst A.LA rd A.ZERO (Left lbl)
li rd imm = iinst A.LI rd A.ZERO (Right imm)
lw rd coff roff = iinst A.LW rd roff (Right coff)
sw rd coff roff = iinst A.SW rd roff (Right coff)
add rd rs rt = rinst A.ADD [rd, rs, rt]
addi rd rs c = iinst A.ADD rd rs (Right c)
sub rd rs rt = rinst A.SUB [rd, rs, rt]
subi rd rs c = iinst A.ADD rd rs (Right c)
mul rd rs rt = rinst A.MUL [rd, rs, rt]
muli rd rs c = iinst A.MUL rd rs (Right c)
div rd rs rt = rinst A.DIV [rd, rs, rt] -- pseudo inst.
slt rd rs rt = rinst A.SLT [rd, rs, rt]
sne rd rs rt = rinst A.SNE [rd, rs, rt]
beq rs rt lbl = iinst A.BEQ rs rt (Left lbl)
bne rs rt lbl = iinst A.BNE rs rt (Left lbl)

xor rd rs rt = rinst A.XOR [rd, rs, rt]
lnot rd rs = iinst A.XOR rd rs (Right 1)
move rd rs = rinst A.ADD [rd, rs, A.ZERO]

j lbl = jinst A.J lbl
jal lbl = jinst A.JAL lbl
jr rd = rinst A.JR [rd]
syscall = rinst A.SYSCALL []

mtc1 rd rs = rinst A.MTC1 [rd, rs]
mfc1 rd rs = rinst A.MFC1 [rd, rs]
ls rd coff roff = iinst A.LS rd roff (Right coff)
ss rd coff roff = iinst A.SS rd roff (Right coff)
moves rd rs = rinst A.MOVES [rd, rs]
cvtws rd rs = rinst A.CVTWS [rd, rs]
cvtsw rd rs = rinst A.CVTSW [rd, rs]
adds rd rs rt = rinst A.ADDS [rd, rs, rt]
subs rd rs rt = rinst A.SUBS [rd, rs, rt]
muls rd rs rt = rinst A.MULS [rd, rs, rt]
divs rd rs rt = rinst A.DIVS [rd, rs, rt]
negs rd rs = rinst A.NEGS [rd, rs]
clts rs rt = rinst A.CLTS [rs, rt]
cles rs rt = rinst A.CLES [rs, rt]
ceqs rs rt = rinst A.CEQS [rs, rt]
bc1t lbl = jinst A.BC1T lbl
bc1f lbl = jinst A.BC1F lbl
saveFlag rd = do
  li rd 0
  bc1t (show 4)
  li rd 1

saveFlagN rd = do
  li rd 1
  bc1f (show 4)
  li rd 0

dataVars lblName = foldl folder []
  where folder xs (L.VarInfo vname vtype) = (lblName vname, A.Space (tySize vtype)):xs

transProg :: L.Prog L.VarInfo -> IO (A.Prog (L.Type, Addr))
transProg (L.Prog globalVars funcs) = A.Prog newData <$> newFuncs <*> pure newVars
  where
    globalVarLabel = ("GLOBAL_VAR_" ++)
    newData = dataVars globalVarLabel globalVars
    newVars = fmap toEntry globalVars
      where toEntry (L.VarInfo vname vtype) = (vtype, AData . globalVarLabel $ vname)
    newFuncs = mapM (transFunc . snd) . toListA $ funcs

    transFunc :: L.Func L.VarInfo -> IO (A.Func (L.Type, Addr))
    transFunc (L.Func fname fargs fvars fentry fcode) =
      A.Func fname newFuncVars newFrameSize <$> newFuncEnter <*> newFuncCode <*> newFuncData
      where
        funcLabel = ((fname ++ "_") ++)
        blockLabel = funcLabel . ("BLK_" ++)
        blockLabel' :: Show a => a -> String
        blockLabel' = blockLabel . show
        localVarLabel = funcLabel . ("VAR_" ++)
        localConstLabel' = funcLabel . ("CONST_" ++). show

        newBlocks = mapM (transBlock . snd) $ toListA fcode
        newFuncCode = (++) <$> (concat <$> fmap (map fst) newBlocks) <*> newFuncReturn
        newFuncData = concat <$> fmap (map snd) newBlocks

        newFuncVars = localVars `unionA` localArgs `unionA` newVars
          where
            folder (acc, idx) (vname, vtype) =
              ((vname, (vtype, AMem idx A.FP)):acc, idx + tySize vtype)
            localArgs = fromListA . fst $ foldl folder ([], 0) fargs

            folder' (acc, idx) (L.VarInfo vname vtype) = (newEntry:acc, idx')
              where
                idx' = idx - tySize vtype
                newEntry = (vname, (vtype, AMem idx' A.FP))
            localVars = fromListA . fst $ foldl folder' ([], 0) fvars

        newFrameSize = sum $ fmap (tySize . snd) fargs

        newFuncEnter = fmap fst . runFoo' emptyA [0] initRegs $ do
          sw A.RA (-4) A.SP
          sw A.FP (-8) A.SP
          move A.FP A.SP
          mapM_ (\x -> sw (A.SReg x) (-12 - 4*x) A.SP) [0..7]
          subi A.SP A.SP (40 + newFrameSize)
          j $ blockLabel' fentry

        newFuncReturn = fmap fst . runFoo' emptyA [0] initRegs $ do
          label (blockLabel "RETURN")
          move A.SP A.FP
          mapM_ (\x -> lw (A.SReg x) (-12 - 4*x) A.SP) [0..7]
          lw A.FP (-8) A.SP
          lw A.RA (-4) A.SP
          if fname == "main"
            then li (A.VReg 0) 10 >> syscall
            else jr A.RA


        spill :: Obj -> Foo ()
        spill x = trace ("spill " ++ show x) $ do
          ns <- getNS
          fidx <- pushFrame
          case snd (ns ! x) of
            AReg rd@(A.FReg _) -> ss rd fidx A.FP
            AReg rd -> sw rd fidx A.FP
          setAddr x (AMem fidx A.FP)

        alloc :: [Obj] -> Foo [A.Reg]
        alloc = trace ("alloc shit") $ mapM mapper
          where
            mapper x = do
              ns <- getNS
              case ns ! x of
                (_, AReg x') -> return x'
                (type', _) -> do
                  q <- getQueue
                  let
                    type'' = getOType (type', "unused field")
                    rightType (A.FReg _) = type'' == OFloat
                    rightType _ = type'' /= OFloat

                    ns' = toListA ns
                    rightTypeQ = filter rightType q
                    regFilter reg = all ((/= (AReg reg)) . snd . snd) ns'
                    freeRegs = filter regFilter rightTypeQ
                    mapUsedRegs reg = head $ filter ((== (AReg reg)) . snd . snd) ns'
                    usedRegsWithOwner = map mapUsedRegs $ filter (not . (`elem` freeRegs)) rightTypeQ
                  case freeRegs of
                    freeReg:_ -> return freeReg
                    [] -> do
                      let (owner, (_, AReg reg)) = head usedRegsWithOwner
                      spill owner
                      requeue reg
                      return reg


        load :: [Obj] -> Foo [A.Reg]
        load xs = trace ("load " ++ show xs) $ do
          ns <- getNS
          let
            xs' = map (snd . (ns !)) xs

            zipper x@(OAddr obj) _ = do
              [rd'] <- alloc [x]
              case obj of
                OVar var -> la rd' var
                OTxt lbl -> la rd' lbl
                _ -> error "MISPTrans.load only supports OVars or OTxts."
              return rd'

            zipper _ (AReg reg) = return reg

            zipper x dat@(AData lbl) = do
              [rd'] <- alloc [x]
              la rd' lbl
              lw rd' 0 rd'
              return rd'
            zipper x mem@(AMem coff roff) = do
              [rd'] <- alloc [x]
              lw rd' coff roff
              return rd'
          zipWithM zipper xs xs'

        finale :: Obj -> Foo ()
        finale x = trace ("finale " ++ show x) $ do
          ns <- getNS
          case snd (ns ! x) of
            AReg _ -> setAddr x AMadoka
            AMem _ rd@(A.FReg idx) | idx < -40-newFrameSize -> do
              popFrame x
              setAddr x AMadoka
            _ -> return ()


        yellow str = "\ESC[33m" ++ str ++ "\ESC[m"

        transBlock :: [L.AST] -> IO ([A.Inst], [A.DataVar])
        transBlock = trace (yellow "[transBlock] ") $ runFoo' emptyA [-40-newFrameSize] initRegs . foldlM transInst 1
          where
            transInst :: Integer -> L.AST -> Foo Integer
            transInst instCount last = trace (yellow "[transInst] " ++ show last ++ "") $ do
              let
                pushLiteral literal = trace ("pushLiteral " ++ show literal) $ do
                  let
                    lbl = (localConstLabel' instCount)
                    addAddr' = \stype -> addAddr (OTxt lbl) stype (AData lbl)
                  case literal of
                    S.IntLiteral    int -> pword   lbl int >> addAddr' S.TInt
                    S.FloatLiteral  flt -> pfloat  lbl flt >> addAddr' S.TFloat
                    S.StringLiteral str -> pstring lbl str >> addAddr' (S.TPtr S.TChar)
                  return lbl

                val2obj val = case val of
                  L.Constant literal -> fmap OTxt $ pushLiteral literal
                  L.Var var -> return $ OVar var
                  L.Reg reg -> return $ OReg reg

              case last of
                (L.Phi rd srcs) -> error "Can I not implement this?"

                (L.Call _ "write" [val]) -> do -- iwrite
                  valo <- val2obj val
                  [rd'] <- load [valo]
                  move (A.AReg 0) rd'
                  li (A.VReg 0) 1
                  syscall
                  finale valo

                (L.Call _ "fwrite" [val]) -> do
                  valo <- val2obj val
                  [rd'] <- load [valo]
                  move (A.FReg 12) rd'
                  li (A.VReg 0) 2
                  syscall
                  finale valo

                (L.Call _ "swrite" [val]) -> do
                  valo <- val2obj val
                  [rd'] <- load [OAddr valo]
                  move (A.AReg 0) rd'
                  li (A.VReg 0) 4
                  syscall
                  finale (OAddr valo)

                (L.Call rd "read" []) -> do
                  li (A.VReg 0) 5
                  syscall
                  [rd'] <- alloc [OReg rd]
                  move rd' (A.VReg 0)

                (L.Call rd "fread" []) -> do
                  li (A.VReg 0) 6
                  syscall
                  [rd'] <- alloc [OReg rd]
                  moves rd' (A.FReg 0)

                (L.Call rd fname args) -> do
                  ns <- getNS

                  let tmpObjs = map fst $ filter ((`elem` initARegs) . snd . snd) (toListA ns)
                  mapM spill tmpObjs

                  let
                    folder coff val = do
                      objToLoad <- val2obj val
                      [rd'] <- load [objToLoad]
                      case rd' of
                        A.FReg _ -> ss rd' (coff-4) A.SP
                        _ -> sw rd' (coff-4) A.SP
                      finale objToLoad
                      return (coff-4)

                  argsSize <- foldlM folder 0 args
                  subi A.SP A.SP argsSize
                  jal fname
                  addi A.SP A.SP argsSize
                  [rd'] <- alloc [OReg rd]
                  move rd' (A.VReg 0)

                (L.Let rd op vals) -> do
                  objs <- mapM val2obj vals
                  xs <- load objs
                  [rd'] <- alloc [OReg rd]
                  setAddr (OReg rd) (AReg rd')
                  ns <- getNS
                  case (head xs, op) of
                    (A.FReg _, L.Negate) -> negs rd' (head xs)
                    (       _, L.Negate) -> sub rd' A.ZERO (head xs)
                    (       _, L.LNot) -> lnot rd' (head xs)
                    (A.FReg _, L.Plus) -> adds rd' (xs !! 0) (xs !! 1)
                    (       _, L.Plus) -> add rd' (xs !! 0) (xs !! 1)
                    (A.FReg _, L.Minus) -> subs rd' (xs !! 0) (xs !! 1)
                    (       _, L.Minus) -> sub rd' (xs !! 0) (xs !! 1)
                    (A.FReg _, L.Times) -> muls rd' (xs !! 0) (xs !! 1)
                    (       _, L.Times) -> mul rd' (xs !! 0) (xs !! 1)
                    (A.FReg _, L.Divide) -> divs rd' (xs !! 0) (xs !! 1)
                    (       _, L.Divide) -> div rd' (xs !! 0) (xs !! 1)

                    (A.FReg _, L.LT) -> clts (xs !! 0) (xs !! 1) >> saveFlag rd'
                    (       _, L.LT) -> slt rd' (xs !! 0) (xs !! 1)
                    (A.FReg _, L.GT) -> clts (xs !! 1) (xs !! 0) >> saveFlag rd'
                    (       _, L.GT) -> slt rd' (xs !! 1) (xs !! 0)
                    (A.FReg _, L.LEQ) -> cles (xs !! 1) (xs !! 0) >> saveFlag rd'
                    (       _, L.LEQ) -> slt rd' (xs !! 1) (xs !! 0) >> lnot rd' rd'
                    (A.FReg _, L.GEQ) -> cles (xs !! 0) (xs !! 1) >> saveFlag rd'
                    (       _, L.GEQ) -> slt rd' (xs !! 0) (xs !! 1) >> lnot rd' rd'
                    (A.FReg _, L.NEQ) -> ceqs (xs !! 0) (xs !! 1) >> saveFlagN rd'
                    (       _, L.NEQ) -> sub rd' (xs !! 0) (xs !! 1)
                    (A.FReg _, L.EQ) -> ceqs (xs !! 0) (xs !! 1) >> saveFlag rd'
                    (       _, L.EQ) -> sub rd' (xs !! 0) (xs !! 1) >> lnot rd' rd'
                    (A.FReg _, L.SetNZ) -> error "Pls don't do this."
                    (       _, L.SetNZ) -> sne rd' (xs !! 0) A.ZERO
                  mapM_ finale objs

                (L.Load rd (Left var)) -> do
                  [rd'] <- load [OVar var]
                  setAddr (OReg rd) (AReg rd')

                (L.Load rd (Right reg)) -> do
                  [rd'] <- alloc [OInt]
                  [reg'] <- load [OReg reg]
                  lw rd' 0 reg'
                  setAddr (OReg rd) (AReg rd')

                (L.Store (Left var) rs) -> do
                  [rs', vara'] <- load [OReg rs, OAddr (OVar var)]
                  sw rs' 0 vara'
                  finale (OAddr (OVar var))
                  finale (OReg rs)

                (L.Store (Right rd) rs) -> do -- mem[rd'] <- rs'
                  [rs', rd'] <- load [OReg rs, OReg rd]
                  sw rs' 0 rd'
                  finale (OReg rs)

                (L.Cast rd S.TInt rs S.TFloat) -> do
                  [rs'] <- load [OReg rs]
                  [rd'] <- alloc [OInt]
                  cvtsw rs' rs'
                  mfc1 rd' rs'
                  finale (OReg rs)

                (L.Cast rd S.TFloat rs S.TInt) -> do
                  [rs'] <- load [OReg rs]
                  [rd'] <- alloc [OFloat]
                  mtc1 rd' rs'
                  cvtws rd' rd'
                  finale (OReg rd)

                (L.ArrayRef rd base idx siz) -> do
                  idxo <- val2obj idx
                  [idx'] <- load [idxo]
                  muli idx' idx' siz
                  let
                    myadd target' srca'@(A.FReg _) srcb' = adds idx' srca' srcb'
                    myadd target' srca' srcb' = add idx' srca' srcb'
                  case base of
                    Left var -> do
                      [vara'] <- load [OAddr (OVar var)]
                      myadd idx' vara' idx'
                      finale (OAddr (OVar var))
                    Right rs -> do
                      [rs'] <- load [OReg rs]
                      myadd idx' rs' idx'
                      finale (OReg rs)
                  [rd'] <- alloc [OInt]
                  lw rd' 0 idx'
                  finale idxo
                  setAddr (OReg rd) (AReg rd')

                (L.Val rd (L.Constant literal)) -> do
                  data' <- pushLiteral literal
                  setAddr (OReg rd) (AData data')

                (L.Branch rd blkTrue blkFalse) -> do
                  [rd'] <- load [OReg rd]
                  -- free (finale) frame variables??
                  bne rd' A.ZERO (blockLabel' blkTrue)
                  j (blockLabel' blkFalse)

                (L.Jump bid) -> j . blockLabel . show $ bid

                (L.Return valueM) -> j $ blockLabel "RETURN"

              return $ instCount + 1
