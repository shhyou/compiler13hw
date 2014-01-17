module Language.BLang.CodeGen.MIPSTrans where

import Prelude hiding (div, seq)
import qualified Data.Foldable as F (foldlM, foldrM, foldMap, foldl)
import Data.List (deleteBy, nub)
import Control.Applicative (Applicative(), (<$>), (<*>), pure)
import Control.Monad (zipWithM, mapM, forM, when)
import Control.Monad.IO.Class

import qualified Language.BLang.BackEnd.LLIR as L
import qualified Language.BLang.CodeGen.AsmIR as A

import Language.BLang.Semantic.Type (tySize)
import Language.BLang.Data
import Language.BLang.Miscellaneous (dirtyLog2, is2pow)

data Obj = OVar String
         | OReg L.Reg
         | OTxt String
         | OTxtInt Integer String
         | OInt
         | OFloat
         | OAddr Obj
         deriving (Show, Eq)

instance Ord Obj where
  (OVar va) <= (OVar vb) = va <= vb
  (OReg ra) <= (OReg rb) = ra <= rb
  (OTxt ta) <= (OTxt tb) = ta <= tb
  (OTxt ta) <= (OTxtInt _ tb) = ta <= tb
  (OTxtInt _ ta) <= (OTxt tb) = ta <= tb
  (OTxtInt _ ta) <= (OTxtInt _ tb) = ta <= tb
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

type NameSpace = Assoc Obj (L.Type, Addr)

getOType :: (L.Type, a) -> Obj
getOType (x, _) = case x of
  L.TFloat -> OFloat
  _ -> OInt

sregs, tregs, fregs, iregs, initRegs :: [A.Reg]
sregs = map A.SReg [0..7]
tregs = map A.TReg [0..9]
fregs = filter (/= (A.FReg 12)) $ map A.FReg [2,4..30]
iregs = tregs ++ sregs
initRegs = iregs ++ fregs

initARegs, tARegs :: [Addr]
initARegs = map AReg initRegs
tARegs = map AReg $ tregs ++ fregs

isFReg :: A.Reg -> Bool
isFReg (A.FReg _) = True
isFReg _ = False


newtype Foo a = Foo (NameSpace ->
                     [Integer] ->
                     [A.Reg] ->
                     [A.Reg] ->
                     (a, [A.Inst], [A.DataVar], NameSpace, [Integer], [A.Reg], [A.Reg]))

makeFoo :: [A.Inst] -> [A.DataVar] -> Foo ()
makeFoo y z = Foo $ \w fs q vst -> ((), y, z, w, fs, q, vst)

getNS :: Foo NameSpace
getNS = Foo $ \ns fs q vst -> (ns, [], [], ns, fs, q, vst)

editNS :: (NameSpace -> NameSpace) -> Foo ()
editNS f = Foo $ \w fs q vst -> ((), [], [], f w, fs, q, vst)

getFrame :: Foo [Integer]
getFrame = Foo $ \ns fs q vst -> (fs, [], [], ns, fs, q, vst)

editFrame :: ([Integer] -> [Integer]) -> Foo ()
editFrame f = Foo $ \ns fs q vst -> ((), [], [], ns, f fs, q, vst)

setFrame :: [Integer] -> Foo ()
setFrame = editFrame . const

frameBottom :: Foo Integer
frameBottom = fmap minimum getFrame

pushFrame :: Foo Integer
pushFrame = do
  frame <- getFrame
  let
    oldHeight = minimum frame
    localVarBot = maximum frame
    idx = head $ filter (not . (`elem` frame)) [localVarBot, localVarBot-4..]
  setFrame (idx:frame)
  when (idx < oldHeight) $
    subi A.SP A.SP (oldHeight - idx)
  return idx

-- does NOT finale the object
popFrame :: Obj -> Foo ()
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
      when (oldHeight < newHeight) $
        addi A.SP A.SP (newHeight - oldHeight)
    _ -> error "Object not in frame"

runFoo :: NameSpace -> [Integer] -> [A.Reg] -> Foo a -> (a, [A.Inst], [A.DataVar], [A.Reg])
runFoo ns fs q (Foo f) =
  let (x, y, z, _, _, _, rs) = f ns fs q []
  in (x, reverse y, reverse z, nub rs)

runFoo' :: NameSpace -> [Integer] -> [A.Reg] -> Foo a -> ([A.Inst], [A.DataVar], [A.Reg])
runFoo' ns fs q = (\(_, y, z, w) -> (y, z, w)) . runFoo ns fs q

instance Functor Foo where
  fmap g (Foo f) = Foo $ \ns fs q vst -> let (x, y, z, w, s, q', vst') = f ns fs q vst
                                     in (g x, y, z, w, s, q', vst')

instance Monad Foo where
  return x = Foo $ \ns fs q vst -> (x, [], [], ns, fs, q, vst)
  (Foo f) >>= g = Foo $ \ns fs q vst ->
    let (x, y, z, ns', fs', q', vst') = f ns fs q vst
        Foo h = g x
        (x', y', z', ns'', fs'', q'', vst'') = h ns' fs' q' vst'
    in (x', y' ++ y, z' ++ z, ns'', fs'', q'', vst'')

rinst :: A.Op -> [A.Reg] -> Foo ()
iinst :: A.Op -> A.Reg -> A.Reg -> Either String Integer -> Foo ()
jinst :: A.Op -> String -> Foo ()
label :: String -> Foo ()

rinst op args = makeFoo [A.RType op args] []
iinst op rd rs imm = makeFoo [A.IType op rd rs imm] []
jinst op imm = makeFoo [A.JType op imm] []
label lbl = makeFoo [A.Label lbl] []

linsts :: [A.Inst] -> Foo ()
linsts xs = makeFoo xs []

pstring :: String -> String -> Foo ()
pword   :: String -> Integer -> Foo ()
pfloat  :: String -> Double -> Foo ()

pstring lbl txt = makeFoo [] [(lbl, A.Text txt)]
pword lbl int = makeFoo [] [(lbl, A.Word [int])]
pfloat lbl dbl = makeFoo [] [(lbl, A.Float [dbl])]


addAddr :: Obj -> L.Type -> Addr -> Foo ()
addAddr rd stype addr = editNS $ insertA rd (stype, addr)
setAddr :: Obj -> Addr -> Foo ()
setAddr obj addr = do
  ns <- getNS
  let (oType, oAddr) = case lookupA obj ns of
        Just x -> x
        Nothing -> error $ "setAddr: " ++ show obj ++ " not in ns"
  editNS $ \_ -> insertA obj (oType, addr) ns

getStack :: Foo [A.Reg]
getStack = Foo $ \ns fs q vst -> (q, [], [], ns, fs, q, vst)

setStack :: [A.Reg] -> Foo ()
setStack newQ = Foo $ \ns fs _ vst -> ((), [], [], ns, fs, newQ, vst)

enstack :: A.Reg -> Foo ()
enstack x = do
  stack <- getStack
  setStack $ x:stack

destack :: A.Reg -> Foo ()
destack x = do
  stack <- getStack
  setStack $ filter (/= x) stack

restack :: A.Reg -> Foo ()
restack x = destack x >> enstack x

regVisit :: A.Reg -> Foo ()
regVisit (A.SReg s) = Foo $ \ns fs q vst -> ((), [], [], ns, fs, q, (A.SReg s):vst)
regVisit _ = return ()

la rd lbl = iinst A.LA rd A.ZERO (Left lbl)
li rd imm = iinst A.LI rd A.ZERO (Right imm)
lw rd coff roff = iinst A.LW rd roff (Right coff)
lw' rd label = iinst A.LW rd A.ZERO (Left label)
sw rd coff roff = iinst A.SW rd roff (Right coff)
sw' rd label = iinst A.SW rd A.ZERO (Left label)
add rd rs rt = rinst A.ADD [rd, rs, rt]
addi rd rs c = iinst A.ADD rd rs (Right c)
sub rd rs rt = rinst A.SUB [rd, rs, rt]
subi rd rs c = addi rd rs (-c)
mul rd rs rt = rinst A.MUL [rd, rs, rt]
div rd rs rt = rinst A.DIV [rd, rs, rt] -- pseudo inst.
sll rd rs h = iinst A.SLL rd rs (Right h) 
slt rd rs rt = rinst A.SLT [rd, rs, rt]
seq rd rs rt = rinst A.SEQ [rd, rs, rt]
sne rd rs rt = rinst A.SNE [rd, rs, rt]
beq rs rt lbl = iinst A.BEQ rs rt (Left lbl)
bne rs rt lbl = iinst A.BNE rs rt (Left lbl)

xor rd rs rt = rinst A.XOR [rd, rs, rt]
lnot rd rs = seq rd rs A.ZERO
move rd rs = rinst A.ADD [rd, rs, A.ZERO]

j lbl = jinst A.J lbl
jal lbl = jinst A.JAL lbl
jr rd = rinst A.JR [rd]
syscall = rinst A.SYSCALL []

mtc1 rd rs = rinst A.MTC1 [rd, rs]
mfc1 rd rs = rinst A.MFC1 [rd, rs]
ls rd coff roff = iinst A.LS rd roff (Right coff)
ls' rd label = iinst A.LS rd A.ZERO (Left label)
ss rd coff roff = iinst A.SS rd roff (Right coff)
ss' rd label = iinst A.SS rd A.ZERO (Left label)
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


dataVars :: (String -> a) -> Assoc String L.VarInfo -> [(a, A.Data)]
dataVars lblName = F.foldl folder []
  where folder xs (L.VarInfo vname vtype) = (lblName vname, A.Space (tySize vtype)):xs

transProg :: L.Prog L.VarInfo -> (A.Prog (L.Type, Addr))
transProg (L.Prog globalVars funcs regs) = A.Prog newData newFuncs newVars
  where
    globalVarLabel = ("GLOBAL_VAR_" ++)
    newData = dataVars globalVarLabel globalVars
    newVars = fmap toEntry globalVars
      where toEntry (L.VarInfo vname vtype) = (vtype, AData . globalVarLabel $ vname)
    newFuncs = map (transFunc . snd) . toListA $ funcs

    regToEntry rd type' = (OReg rd, (type', AVoid))
    regNS = fromListA . map (uncurry regToEntry) $ toListA regs
    varToEntry _ (L.VarInfo vname vtype) = (OVar vname, (vtype, AData . globalVarLabel $ vname))
    globalVarNS = fromListA . map (uncurry varToEntry) $ toListA globalVars

    globalNS = globalVarNS `unionA` regNS

    transFunc :: L.Func L.VarInfo -> (A.Func (L.Type, Addr))
    transFunc (L.Func fname fargs fvars' fentry fcode) =
      A.Func fname newFuncVars newFrameSize newFuncEnter newFuncCode newFuncData
      where
        fvars = filterA ((`notElem` (map fst fargs)) . L.varName) fvars'

        funcLabel = ((fname ++ "_") ++)
        blockLabel = funcLabel . ("BLK_" ++)
        blockLabel' :: Show a => a -> String
        blockLabel' = blockLabel . show
        localVarLabel = funcLabel . ("VAR_" ++)

        newBlocks = map (\(lbl, code) -> transBlock lbl code) $ toListA fcode
        saveRegs = nub $ map (\(A.SReg r) -> r) $ concatMap (\(_, _, rs) -> rs) newBlocks
        newFuncCode = concatMap (\(a, _, _) -> a) newBlocks ++ newFuncReturn
        newFuncData = concatMap (\(_, b, _) -> b) newBlocks

        calleeSaveRegSize = 40

        newFuncVars = localVars `unionA` localArgs `unionA` newVars
          where
            folder (acc, idx) (vname, vtype) =
              ((vname, (vtype, AMem idx A.FP)):acc, idx + tySize vtype)
            localArgs = fromListA . fst $ F.foldl folder ([], 0) fargs

            folder' (acc, idx) (L.VarInfo vname vtype) = (newEntry:acc, idx')
              where
                idx' = idx - tySize vtype
                newEntry = (vname, (vtype, AMem idx' A.FP))
            localVars = fromListA . fst $ F.foldl folder' ([], -calleeSaveRegSize) fvars

        localNS = fromListA . map (\(x, y) -> (OVar x, y)) . toListA $ newFuncVars
        newFuncNS = localNS `unionA` globalNS

        newFrameSize = sum . fmap (\(_, L.VarInfo _ ty) -> tySize ty) . toListA $ fvars

        newFuncEnter = (\(a, _, _) -> a) . runFoo' emptyA [0] initRegs $ do
          sw A.RA (-4) A.SP
          sw A.FP (-8) A.SP
          move A.FP A.SP
          mapM_ (\x -> sw (A.SReg x) (-12 - 4*x) A.SP) saveRegs
          subi A.SP A.SP (calleeSaveRegSize + newFrameSize)
          j $ blockLabel' fentry

        newFuncReturn = (\(a, _, _) -> a) . runFoo' emptyA [0] initRegs $ do
          label (blockLabel "RETURN")
          move A.SP A.FP
          mapM_ (\x -> lw (A.SReg x) (-12 - 4*x) A.SP) (reverse saveRegs)
          lw A.FP (-8) A.SP
          lw A.RA (-4) A.SP
          jr A.RA


        spill :: Obj -> Foo ()
        spill x = do
          ns <- getNS
          fidx <- pushFrame
          case snd (ns ! x) of
            AReg rd@(A.FReg _) -> ss rd fidx A.FP
            AReg rd -> sw rd fidx A.FP
          setAddr x (AMem fidx A.FP)

        alloc :: [Obj] -> Foo [A.Reg]
        alloc xs = mapM mapper xs
          where
            getMipsReg isFloat = do
              ns <- getNS
              q <- getStack
              let
                rightType (A.FReg _) = isFloat
                rightType _ = not isFloat

                ns' = toListA ns
                rightTypeQ = filter rightType q
                regFilter reg = all ((/= (AReg reg)) . snd . snd) ns'
                freeRegs = filter regFilter rightTypeQ
                mapUsedRegs reg = head $ filter ((== (AReg reg)) . snd . snd) ns'
                usedRegsWithOwner = map mapUsedRegs $ filter (not . (`elem` freeRegs)) rightTypeQ
              case freeRegs of
                freeReg:_ -> regVisit freeReg >> restack freeReg >> return freeReg
                [] -> do
                  let (owner, (_, AReg reg)) = head usedRegsWithOwner
                  spill owner
                  restack reg
                  return reg

            isOFloat x = case x of { OFloat -> True; _ -> False; }

            mapper x = do
              ns <- getNS
              case x `lookupA` ns of
                Nothing ->
                  case x of
                    OAddr _ -> do
                      mipsReg <- getMipsReg False
                      setAddr x (AReg mipsReg)
                      return mipsReg
                    OInt -> getMipsReg False
                    OFloat -> getMipsReg True
                    _ -> error $ "alloc: could not find '" ++ show x ++ "' in ns"
                Just (_, AReg x') -> return x'
                Just (type', _) -> do
                  mipsReg <- getMipsReg . isOFloat $ getOType (type', "unused field")
                  setAddr x (AReg mipsReg)
                  return mipsReg


        load :: [Obj] -> Foo [A.Reg]
        load xs = do
          ns <- getNS
          let
            xs' = map (snd . (ns !)) xs

            loadVarAddr rd' var = case snd (newFuncVars ! var) of
              AData lbl -> la rd' lbl
              AMem coff roff -> addi rd' roff coff
              _ -> error $ "locaVarAddr: variable '" ++ show var ++ "' is not in data or mem"

            -- do not change order or ns!(OAddr _), ns!(OImmInt _) will be evaluated and will raise an error
            zipper x@(OAddr obj) _ =
              case lookupA x ns of
                Just (_, AReg reg) -> return reg
                _ -> do
                  addAddr x (L.TPtr L.TInt) AVoid  -- the type is not important (?
                  [rd'] <- alloc [x]
                  case obj of
                    OVar var -> loadVarAddr rd' var
                    OTxt lbl -> la rd' lbl
                    _ -> error "MISPTrans.load only supports OVars or OTxts."
                  return rd'
            zipper _ (AReg reg) = return reg
            zipper x@(OTxtInt imm _) _ = do
              [rd'] <- alloc [x]
              li rd' imm
              return rd'
            zipper x dat@(AData lbl) = do -- x is OTxt or OReg?
              [rd'] <- alloc [x]          -- OReg for GLOBAL_VAR_xxx
              case getOType (ns ! x) of
                OFloat -> ls' rd' lbl
                _      -> lw' rd' lbl
              return rd'
            zipper x mem@(AMem coff roff) = do
              [rd'] <- alloc [x]
              let loadCmd = case getOType (ns ! x) of { OFloat -> ls; _ -> lw; }
              loadCmd rd' coff roff
              return rd'
            zipper x AVoid = error $ "load.zipper: '" ++ show x ++ "' -> AVoid"
            zipper x AMadoka = error $ "load.zipper: '" ++ show x ++ "' -> AMadoka"
            -- zipper x y = error $ "load.zipper: non-exhaustive pattern (" ++ show x ++ ") (" ++ show y ++ ")" -- overlapped
          zipWithM zipper xs xs'

        finale :: Obj -> Foo ()
        finale x = do
          ns <- getNS
          case snd (ns ! x) of
            AReg _ -> setAddr x AMadoka
            AMem idx A.FP | idx < -calleeSaveRegSize-newFrameSize -> do
              popFrame x
              setAddr x AMadoka
            other -> error $ "Finale: " ++ show x ++ "@" ++ show other


        yellow str = "\ESC[33m" ++ str ++ "\ESC[m"

        transBlock :: L.Label -> [L.AST] -> ([A.Inst], [A.DataVar], [A.Reg])
        transBlock blkLbl = runFooWithArgs . (label (blockLabel' blkLbl) >>) . F.foldlM transInst 1
          where
            localConstLabel = funcLabel . ((show blkLbl ++ "_CONST_") ++)
            runFooWithArgs = runFoo' newFuncNS [-calleeSaveRegSize-newFrameSize] initRegs

            transInst :: Integer -> L.AST -> Foo Integer
            transInst instCount last = do
              let
                pushLiteral literal = do
                  ns <- getNS
                  let
                    nsSize = length $ toListA ns
                    lbl = localConstLabel $ show instCount ++ "_" ++ show nsSize
                    addAddr' = \stype -> addAddr (OTxt lbl) stype (AData lbl)
                  case literal of
                    L.IntLiteral    int -> addAddr (OTxtInt int lbl) L.TInt (AData lbl) -- phantom label
                    L.FloatLiteral  flt -> pfloat  lbl flt >> addAddr' L.TFloat
                    L.StringLiteral str -> pstring lbl str >> addAddr' (L.TPtr L.TChar)
                  return lbl

                val2obj val = case val of
                  L.Constant literal@(L.IntLiteral imm) -> fmap (OTxtInt imm) $ pushLiteral literal
                  L.Constant literal -> fmap OTxt $ pushLiteral literal
                  L.Var var -> return $ OAddr (OVar var)
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
                  moves (A.FReg 12) rd'
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

                  let tmpObjs = map fst $ filter ((`elem` tARegs) . snd . snd) (toListA ns)
                  mapM spill tmpObjs

                  let
                    folder val coff = do
                      objToLoad <- val2obj val
                      [rd'] <- load [objToLoad]
                      case rd' of
                        A.FReg _ -> ss rd' (coff-4) A.SP
                        _ -> sw rd' (coff-4) A.SP
                      finale objToLoad
                      return (coff-4)

                  argsOffset <- F.foldrM folder 0 args -- it's negative
                  addi A.SP A.SP argsOffset
                  jal fname
                  addi A.SP A.SP (-argsOffset)
                  case fst $ ns ! (OReg rd) of
                    L.TVoid -> return ()
                    L.TFloat -> do
                      [rd'] <- alloc [OReg rd]
                      moves rd' (A.FReg 0)
                    _ -> do
                      [rd'] <- alloc [OReg rd]
                      move rd' (A.VReg 0)

                (L.Let rd op vals) -> do
                  let
                    flagLabel = funcLabel $ show blkLbl ++ "_FLG_" ++ show instCount

                    saveFlag' :: (String -> Foo ()) -> A.Reg -> Foo ()
                    saveFlag' bc1X rd = do
                      li rd 1
                      bc1X flagLabel
                      li rd 0
                      label flagLabel

                    saveFlag :: A.Reg -> Foo ()
                    saveFlag = saveFlag' bc1t

                    saveFlagN :: A.Reg -> Foo ()
                    saveFlagN = saveFlag' bc1f

                  objs <- mapM val2obj vals
                  xs <- load objs
                  [rd'] <- alloc [OReg rd]
                  case (head xs, op) of
                    (A.FReg _, L.Negate) -> negs rd' (head xs)
                    (       _, L.Negate) -> sub rd' A.ZERO (head xs)
                    (A.FReg _, L.LNot) -> do [fz'] <- alloc [OFloat]
                                             mtc1 A.ZERO fz' -- no need to convert $0
                                             ceqs (xs !! 0) fz'
                                             saveFlag rd'
                    (      _, L.LNot) -> lnot rd' (head xs)
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
                    (A.FReg _, L.LEQ) -> cles (xs !! 0) (xs !! 1) >> saveFlag rd'
                    (       _, L.LEQ) -> slt rd' (xs !! 1) (xs !! 0) >> lnot rd' rd'
                    (A.FReg _, L.GEQ) -> cles (xs !! 1) (xs !! 0) >> saveFlag rd'
                    (       _, L.GEQ) -> slt rd' (xs !! 0) (xs !! 1) >> lnot rd' rd'
                    (A.FReg _, L.NEQ) -> ceqs (xs !! 0) (xs !! 1) >> saveFlagN rd'
                    (       _, L.NEQ) -> sne rd' (xs !! 0) (xs !! 1)
                    (A.FReg _, L.EQ) -> ceqs (xs !! 0) (xs !! 1) >> saveFlag rd'
                    (       _, L.EQ) -> seq rd' (xs !! 0) (xs !! 1)
                    (A.FReg _, L.SetNZ) -> do [fz'] <- alloc [OFloat]
                                              mtc1 A.ZERO fz' -- no need to convert $0
                                              ceqs (xs !! 0) fz'
                                              saveFlagN rd'
                    (       _, L.SetNZ) -> sne rd' (xs !! 0) A.ZERO
                  mapM_ finale objs

                (L.Load rd (Left var)) -> do
                  [rd'] <- load [OVar var]
                  setAddr (OReg rd) (AReg rd')
                  setAddr (OVar var) (snd (newFuncVars ! var))

                (L.Load rd (Right reg)) -> do
                  ns <- getNS
                  let rdType = getOType (ns ! (OReg rd))
                  [rd'] <- alloc [rdType]
                  [reg'] <- load [OReg reg]
                  case rdType of
                    OFloat -> ls rd' 0 reg'
                    OInt -> lw rd' 0 reg'
                    _ -> error $ "L.Load: wierd type " ++ show rdType ++ " of " ++ show rd
                  setAddr (OReg rd) (AReg rd')

                (L.Store (Left var) rs) -> do
                  [rs'] <- load [OReg rs]
                  let (store, store') = case rs' of
                        A.FReg _ -> (ss, ss')
                        _        -> (sw, sw')
                  case snd (newFuncVars ! var) of
                    AData lbl -> store' rs' lbl
                    AMem coff roff -> store rs' coff roff
                    a -> error $ "Unknown L.Store (" ++ show a ++ ") " ++ show rs
                  when (var /= "short_circuit_tmp") $ finale (OReg rs)
                  setAddr (OVar var) (snd (newFuncVars ! var))

                (L.Store (Right rd) rs) -> do -- mem[rd'] <- rs'
                  [rs', rd'] <- load [OReg rs, OReg rd]
                  case rs' of
                    A.FReg _ -> ss rs' 0 rd'
                    _ -> sw rs' 0 rd'
                  finale (OReg rs)

                (L.Cast rd L.TInt rs L.TFloat) -> do
                  [rs'] <- load [OReg rs]
                  [rd'] <- alloc [OInt]
                  cvtws rs' rs'
                  mfc1 rd' rs'
                  finale (OReg rs)
                  setAddr (OReg rd) (AReg rd')

                (L.Cast rd L.TFloat rs L.TInt) -> do
                  [rs'] <- load [OReg rs]
                  [rd'] <- alloc [OFloat]
                  mtc1 rs' rd'
                  cvtsw rd' rd'
                  finale (OReg rs)
                  setAddr (OReg rd) (AReg rd')

                (L.Cast _ ty' _ ty) ->
                  error $ "Casting type " ++ show ty' ++ " to " ++ show ty

                (L.ArrayRef rd base (L.Constant (L.IntLiteral idx)) siz)
                  | 0 <= idx*siz && idx*siz < 65536 -> do
                  [rd'] <- alloc [OInt]
                  case base of
                    Left var -> do
                      [vara'] <- load [OAddr (OVar var)]
                      when (idx*siz /= 0 || rd' /= vara') (addi rd' vara' (idx*siz))
                      finale (OAddr (OVar var))
                    Right rs -> do
                      [rs'] <- load [OReg rs]
                      when (idx*siz /= 0 || rd' /= rs') (addi rd' rs' (idx*siz))
                      finale (OReg rs)
                  setAddr (OReg rd) (AReg rd')

                (L.ArrayRef rd base idx siz)
                  | is2pow siz -> do
                  let logsiz = dirtyLog2 siz
                  idxo <- val2obj idx
                  [idx'] <- load [idxo]
                  [rd'] <- alloc [OInt]
                  sll idx' idx' (toInteger $ logsiz `mod` 32)
                  case base of
                    Left var -> do
                      [vara'] <- load [OAddr (OVar var)]
                      add rd' vara' idx'
                      finale (OAddr (OVar var))
                    Right rs -> do
                      [rs'] <- load [OReg rs]
                      add rd' rs' idx'
                      finale (OReg rs)
                  finale idxo
                  setAddr (OReg rd) (AReg rd')

                (L.ArrayRef rd base idx siz) -> do
                  idxo <- val2obj idx
                  sizo <- val2obj (L.Constant (L.IntLiteral siz))
                  [idx', siz'] <- load [idxo, sizo]
                  [rd'] <- alloc [OInt]
                  mul idx' idx' siz'
                  case base of
                    Left var -> do
                      [vara'] <- load [OAddr (OVar var)]
                      add rd' vara' idx'
                      finale (OAddr (OVar var))
                    Right rs -> do
                      [rs'] <- load [OReg rs]
                      add rd' rs' idx'
                      finale (OReg rs)
                  finale idxo
                  finale sizo
                  setAddr (OReg rd) (AReg rd')

                (L.Val rd val) -> do
                  valo <- val2obj val
                  [rd'] <- load [valo]
                  setAddr (OReg rd) (AReg rd')

                (L.Branch rd blkTrue blkFalse) -> do
                  [rd'] <- load [OReg rd]
                  bne rd' A.ZERO (blockLabel' blkTrue)
                  j (blockLabel' blkFalse)

                (L.Jump bid) -> j . blockLabel . show $ bid

                (L.Return Nothing) -> j $ blockLabel "RETURN"
                (L.Return (Just val)) -> do
                  valObj <- val2obj val
                  [rd'] <- load [valObj]
                  case rd' of
                    A.FReg _ -> moves (A.FReg 0) rd'
                    _ -> move (A.VReg 0) rd'
                  j $ blockLabel "RETURN"

              return $ instCount + 1
