module Language.BLang.CodeGen.MIPSTrans where

import Prelude hiding (div, foldl)
import Data.Foldable (foldlM, foldMap, foldl)
import Data.List (deleteBy)
import Control.Monad (zipWithM)

import qualified Language.BLang.Semantic.AST as S
import qualified Language.BLang.CodeGen.LLIR as L
import qualified Language.BLang.CodeGen.AsmIR as A

import Language.BLang.Semantic.Type (tySize)
import Language.BLang.Data


-- TODO: Add OAddr (OPtr?)
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
fregs = map A.FReg [0,2..30]

regsNotIn :: NameSpace -> [A.Reg] -> [A.Reg]
regsNotIn ns regs = foldl folder regs ns
  where
    folder xs (_, (AReg reg)) = deleteBy (==) reg regs
    folder xs _ = xs


newtype Foo a = Foo (NameSpace -> (a, [A.Inst], [A.DataVar], NameSpace))

makeFoo :: [A.Inst] -> [A.DataVar] -> Foo ()
makeFoo y z = Foo $ \w -> ((), y, z, w)

getNS = Foo $ \ns -> (ns, [], [], ns)
editNS f = Foo $ \w -> ((), [], [], f w)

runFoo :: NameSpace -> Foo a -> (a, [A.Inst], [A.DataVar])
runFoo ns (Foo f) = (x, reverse y, reverse z)
  where (x, y, z, _) = f ns
runFoo' ns = (\(_, y, z) -> (y, z)) . runFoo ns

instance Functor Foo where
  fmap g (Foo f) = Foo $ \ns -> let (x, y, z, w) = f ns
                                in (g x, y, z, w)

instance Monad Foo where
  return x = Foo $ \ns -> (x, [], [], ns)
  (Foo f) >>= g = Foo $ \ns ->
    let
      (x, y, z, ns') = f ns
      (Foo h) = g x
      (x', y', z', ns'') = h ns'
    in (x', y' ++ y, z' ++ z, ns'')

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
setAddr rd addr = editNS $ \ns -> insertA rd (fst $ ns ! rd, addr) ns

-- FUCK FLOATING POINTS
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
xor rd rs rt = rinst A.XOR [rd, rs, rt]
lnot rd rs = iinst A.XOR rd rs (Right 1)
move rd rs = rinst A.ADD [rd, rs, A.ZERO]
beq rs rt lbl = iinst A.BEQ rs rt (Left lbl)
bne rs rt lbl = iinst A.BNE rs rt (Left lbl)

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


dataVars lblName = foldl folder []
  where folder xs (L.VarInfo vname vtype) = (lblName vname, A.Space (tySize vtype)):xs

transProg :: L.Prog L.VarInfo -> A.Prog (L.Type, Addr)
transProg (L.Prog funcs globalVars regData) = A.Prog newData newFuncs newVars
  where
    -- funcs :: Assoc String (L.Func L.VarInfo)
    -- globalVars :: Assoc String (L.VarInfo)
    -- regData :: Assoc L.Reg L.RegInfo
    -- newData :: [(String, A.Data)]
    -- newFuncs :: [A.Func v]
    -- newVars :: Assoc String (L.Type, Addr)   <-- what is this for?

    globalVarLabel = ("GLOBAL_VAR_" ++)
    newData = dataVars globalVarLabel globalVars
    newVars = fmap toEntry globalVars
      where toEntry (L.VarInfo vname vtype) = (vtype, AData . globalVarLabel $ vname)
    newFuncs = undefined

    transFunc :: L.Func L.VarInfo -> A.Func (L.Type, Addr)
    transFunc (L.Func fname fargs fvars fentry fcode) =
      A.Func fname newFuncVars newFrameSize newFuncEnter newFuncCode newFuncData
      where
        -- fname :: String
        -- fargs :: [(String, S.Type)]
        -- fvars :: Assoc String L.VarInfo
        -- fentry :: Int <- should be L.Label
        -- fcode :: Assoc L.Label [L.AST]
        -- newFuncVars :: Assoc String v  <-- what?
        -- newFrameSize :: Int
        -- newFuncEnter :: [A.Inst]
        -- newFuncCode :: [A.Inst]
        -- newFuncData :: [A.DataVar]

        funcLabel = ((fname ++ "_") ++)
        blockLabel = funcLabel . ("BLK_" ++)
        blockLabel' :: Show a => a -> String
        blockLabel' = blockLabel . show
        localVarLabel = funcLabel . ("VAR_" ++)
        localConstLabel' = funcLabel . ("CONST_" ++). show

        newFuncCode = undefined

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
        newFuncData = dataVars localVarLabel fvars

        newFuncEnter = fst . runFoo' emptyA $ do
          sw A.RA (-4) A.SP
          sw A.FP (-8) A.SP
          move A.FP A.SP
          mapM_ (\x -> sw (A.SReg x) (-12 - 4*x) A.SP) [0..7]
          subi A.SP A.SP (40 + newFrameSize)
          j $ blockLabel' fentry


        spill :: Obj -> Foo ()
        spill x = undefined

        alloc :: [Obj] -> Foo [A.Reg]
        alloc xs = do
          ns <- getNS
          let xs' = map (getOType . (ns !)) xs
          ys' <- undefined
          zipWithM (\x y -> setAddr x (AReg y)) xs ys'
          return ys'

        load :: [Obj] -> Foo [A.Reg]
        load xs = do
          ns <- getNS
          let
            xs' = map (snd . (ns !)) xs
            zipper (AReg reg) y = return reg
            zipper (AData lbl) y = do
              undefined
            zipper (AMem coff roff) y = do
              undefined
          ys' <- alloc xs
          zipWithM zipper xs' ys'
          return ys'

        loadTo :: Obj -> A.Reg -> Foo ()
        loadTo (OVar var) rd = undefined
        loadTo (OReg reg) rd = undefined
        loadTo (OTxt lbl) rd = undefined

        finale :: Obj -> Foo ()
        finale x = do
          ns <- getNS
          case snd (ns ! x) of
            AReg _ -> setAddr x AMadoka
            _ -> return ()


        transBlock :: [L.AST] -> ([A.Inst], [A.DataVar])
        transBlock = runFoo' emptyA . foldlM transInst 1
          where
            transInst :: Integer -> L.AST -> Foo Integer
            transInst instCount last = do
              let
                pushLiteral literal = do
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

              -- return transInst instCount last
              case last of
                (L.Phi rd srcs) -> error "Can I not implement this?"

                (L.Call rd "write" [val]) -> do
                  ns <- getNS
                  valo <- val2obj val
                  case fst (ns ! valo) of
                    S.TInt -> do
                      loadTo valo (A.AReg 0)
                      li (A.VReg 0) 1
                    S.TFloat -> do
                      loadTo valo (A.FReg 12)
                      li (A.VReg 0) 2
                    S.TPtr S.TChar -> do
                      loadTo (OAddr valo) (A.AReg 0)
                      li (A.VReg 0) 4
                  syscall

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

                (L.Call rd fname args) -> undefined

                (L.Let rd op vals) -> do
                  objs <- mapM val2obj vals
                  xs <- load objs  -- will this step fail?
                  [rd'] <- alloc [OReg rd]
                  setAddr (OReg rd) (AReg rd')
                  case op of
                    L.Negate -> sub rd' A.ZERO (head xs)
                    L.LNot -> lnot rd' (head xs)
                    L.Plus -> add rd' (xs !! 0) (xs !! 1)
                    L.Minus -> sub rd' (xs !! 0) (xs !! 1)
                    L.Times -> mul rd' (xs !! 0) (xs !! 1)
                    L.Divide -> div rd' (xs !! 0) (xs !! 1)
                    L.LT -> slt rd' (xs !! 0) (xs !! 1)
                    L.GT -> slt rd' (xs !! 1) (xs !! 0)
                    L.LEQ -> slt rd' (xs !! 1) (xs !! 0) >> lnot rd' rd'
                    L.GEQ -> slt rd' (xs !! 0) (xs !! 1) >> lnot rd' rd'
                    L.NEQ -> sub rd' (xs !! 0) (xs !! 1)
                    L.EQ -> sub rd' (xs !! 0) (xs !! 1) >> lnot rd' rd'
                    L.SetNZ -> sne rd' (xs !! 0) A.ZERO
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
                  [rs'] <- load [OReg rs]
                  [rd'] <- load [OReg rd]
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
                  case base of
                    Left var -> do
                      [vara'] <- load [OAddr (OVar var)]
                      add idx' vara' idx'
                      finale (OAddr (OVar var))
                    Right rs -> do
                      [rs'] <- load [OReg rs]
                      add idx' rs' idx'
                      finale (OReg rs)
                  -- FUCK FLOATING POINTS
                  [rd'] <- alloc [OInt]
                  lw rd' 0 idx'
                  finale idxo
                  setAddr (OReg rd) (AReg rd')

                (L.Val rd (L.Constant literal)) -> do
                  data' <- pushLiteral literal
                  setAddr (OReg rd) $ AData data'

                (L.Branch rs blkTrue blkFalse) -> do
                  loadTo (OReg rs) (A.TReg 0)
                  bne (A.TReg 0) A.ZERO (blockLabel' blkTrue)
                  j (blockLabel' blkFalse)

                (L.Jump bid) -> j . blockLabel . show $ bid

                (L.Return valueM) -> j $ blockLabel "RETURN"
              return $ instCount + 1
