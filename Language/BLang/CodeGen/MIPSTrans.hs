module Language.BLang.CodeGen.MIPSTrans where

import Prelude hiding (div)

import qualified Language.BLang.Semantic.AST as S
import qualified Language.BLang.CodeGen.LLIR as L
import qualified Language.BLang.CodeGen.AsmIR as A

import Language.BLang.Semantic.Type (tySize)
import Language.BLang.Data


data Obj = OVar String
         | OReg Int
         | OTxt String
         | OInt
         | OFloat
         deriving (Show)

instance Ord Obj where
  (OVar va) <= (OVar vb) = va <= vb
  (OReg ra) <= (OReg vb) = ra <= rb
  (OTxt ta) <= (OTxt tb) = ta <= tb
  (OVar _) <= _ = True
  (OReg _) <= _ = True
  (OTxt _) <= _ = True
  (OInt _) <= _ = True
  (OFloat _) <= _ = True
  _ <= _ = False

data Addr = AReg A.Reg
          | AData String
          | AMem Int A.Reg
          | AVoid
          | AMadoka
          deriving (Show, Eq)

type NameSpace = Assoc Obj (S.Type, Addr)

getOType (x, _) = case x of
  S.TFloat -> OFloat
  _ -> OInt

iregs = map A.SReg [0..7] ++ map A.TReg [0..9]
fregs = map A.FReg [0,2..30]

regsNotIn ns regs = foldl folder regs ns
  where
    folder xs (_, (AReg reg)) = deleteBy (reg ==) regs
    folder xs _ = xs

regContent ns reg = undefined


newtype Foo a = Foo (a, [A.Inst], [A.DataVar], NameSpace) deriving (Show)

runFoo :: Foo a -> (a, [A.Inst], [A.DataVar])
runFoo (Foo (x, y, z, _)) = (x, reverse y, reverse z)
runFoo' (Foo (_, y, z, _) = (reverse y, reverse z)
ns (Foo (_, _, _, ret)) = ret

instance Functor (Foo a) where
  fmap f (Foo (x, y, z, w)) = Foo (f x, y, z, w)

instance Monad (Foo a) where
  return x = Foo (x, [], [], emptyA)
  (Foo (x, y, z, w)) >>= f =
    let (Foo (x', y', z', w')) = f x
    in Foo (x', y' ++ y, z' ++ z, w' `unionA` w)

rinst op args = Foo ((), [A.RType op args], [], emptyA)
iinst op rd rs imm = Foo ((), [A.IType op rd rs imm], [], emptyA)
jinst op imm = Foo ((), [A.JType op imm], [], emptyA)
label lbl = Foo ((), [A.Label lbl], [], emptyA)

linsts xs = Foo ((), xs, [], emptyA)

ptext lbl txt = Foo ((), [], [(lbl, A.Text txt)], emptyA)
pword lbl int = Foo ((), [], [(lbl, A.Word [int])], emptyA)
pfloat lbl dbl = Foo ((), [], [(lbl, A.Float [dbl])], emptyA)

setAddr rd addr = Foo (addr, [], [], Assoc [(rd, addr)])

-- FUCK FLOATING POINTS
la rd lbl = iinst A.LA rd A.ZERO (Left lbl)
li rd imm = iinst A.LI rd A.ZERO (Right imm)
lw rd coff roff = iinst A.LW rd roff (Right coff)
sw rd coff roff = iinst A.SW rd roff (Right coff)
add rd rs rt = rinst A.ADD [rd, rs, rt]
sub rd rs rt = rinst A.SUB [rd, rs, rt]
mul rd rs rt = rinst A.MUL [rd, rs, rt]
div rd rs rt = rinst A.DIV [rd, rs, rt] -- pseudo inst.
slt rd rs rt = rinst A.SLT [rd, rs, rt]
sne rd rs rt = rinst A.SNE [rd, rs, rt]
xor rd rs rt = rinst A.XOR [rd, rs, rt]
lnot rd rs = iinst A.XOR rd rs (Right 1)
move rd rs = rinst A.ADD [rd, rs, A.ZERO]
beq rs rt lbl = iinst A.BEQ rs rt (Left lbl)
bne rd rt lbl = iinst A.BNE rs rt (Left lbl)

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

ransProg :: L.Prog L.VarInfo -> A.Prog v
transProg (L.Prog funcs globalVars regData) = A.Prog newData newFuncs newVars
  where
    -- funcs :: Assoc String (L.Func L.VarInfo)
    -- globalVars :: Assoc String (L.VarInfo)
    -- regData :: Assoc L.Reg L.RegInfo
    -- newData :: [(String, Data)]
    -- newFuncs :: [A.Func v]
    -- newVars :: Assoc String v    <- don't know what this is for

    globalVarLabel = ("GLOBAL_VAR_" ++)
    newData = dataVars globalVarLabel globalVars
    newVars = map (\vname vtype -> (vtype, AData . globalVarLabel $ vname)) globalVars
    newFuncs = undefined

    transFunc :: L.Func L.VarInfo -> A.Func v
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

        funcLabel = (fname ++ "_" ++)
        blockLabel = funcLabel . ("BLK_" ++)
        blockLabel' = blockLabel . show
        localVarLabel = funcLabel . ("VAR_" ++)
        localConstLabel' = funcLabel . ("CONST_" ++). show

        newFuncVars = localVars `unionA` localArgs `unionA` globalVars
          where
            folder (acc, idx) (vname, vtype) = ((vtype, AMem idx A.FP):acc, idx + tySize vtype)
            localArgs = Assoc . fst $ foldl folder ([], 0) fargs

            folder' (acc, idx) (L.VarInfo vname vtype) = ((vtype, AMem idx' A.FP):acc, idx')
              where idx' = idx - tySize vtype
            localVars = Assoc . fst $ foldl folder' ([], 0) fvars

        newFrameSize = sum $ fmap (tySize . L.varType) fargs
        newFuncData = dataVars localVarLabel fvars

        newFuncEnter = fst . runFoo' $ do
          sw A.RA A.SP -4
          sw A.FP A.SP -8
          move A.FP A.SP
          mapM_ (\x -> sw (A.SReg x) A.SP (-12 - 4*x)) [0..7]
          sub A.SP A.SP (40 + newFrameSize)
          j $ blockLabel' fentry


        spill :: Obj -> NameSpace -> Foo ()
        spill x ns = undefined

        alloc :: Obj -> NameSpace -> Foo A.Reg
        alloc OInt ns = case regsNotIn ns iregs of
          [] -> undefined
          x:_ -> return x
        alloc OFloat ns = case regsNotIn ns fregs of
          [] -> undefined
          x:_ -> return x
        alloc x ns = do
          rd <- alloc . getOType $ ns ! x
          setAddr x (AReg rd)
          return rd

        -- loads ??
        load :: Obj -> NameSpace -> Foo A.Reg
        load x ns = case ns ! x of
          AReg reg -> return reg
          els -> do
            reg <- alloc x ns
            case els of
              AData lbl -> undefined
              AMem coff roff -> undefined
            return reg

        loadTo :: Obj -> A.Reg -> NameSpace -> Foo ()
        loadTo (OVar var) rd ns = undefined
        loadTo (OReg reg) rd ns = undefined
        loadTo (OTxt lbl) rd ns = undefined

        finale :: Obj -> NameSpace -> Foo ()
        finale x ns = setAddr x TMadoka


        transBlock :: [L.AST] -> ([A.Inst], [A.DataVar])
        transBlock = runFoo' . foldlM transInst (return 1)
          where
            transInst :: Int -> L.AST -> Foo Int
            transInst instCount last = do
              let
                pushLiteral literal = do
                  let lbl = (localConstLabel' instCount)
                  case literal of
                    S.IntLiteral    int -> pword   lbl int
                    S.FloatLiteral  flt -> pfloat  lbl flt
                    S.StringLiteral str -> pstring lbl str
                  return lbl

                val2obj val = case val of
                  L.Constant literal -> pushLiteral literal >>= OTxt
                  L.Var var -> return $ OVar var
                  L.Reg reg -> return $ OReg reg

              -- return transInst instCount last
              case last of
                (L.Phi rd srcs) -> error "Can I not implement this?"
                (L.Call rd "write" [x]) -> case x of _ -> undefined
                (L.Call rd "read" []) -> do
                  lw (A.VReg 0) 5
                  syscall
                  rd' <- ns>>=alloc rd
                  move rd' (A.VReg 0)

                (L.Call rd "fread" []) -> do
                  lw (A.VReg 0) 6
                  syscall
                  rd' <- ns>>=alloc rd
                  moves rd' (A.FReg 0)

                (L.Call rd fname args) -> undefined

                (L.Let rd op vals) -> do
                  objs <- mapM val2obj vals
                  xs <- map (ns>>=load) objs  -- will this step fail?
                  rd' <- alloc rd
                  setAddr rd (AReg rd')
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

                (L.Load rd (Left var)) -> undefined
                (L.Load rd (Right reg)) -> undefined
                (L.Store (Left var) rs) -> undefined
                (L.Store (Right rd) rs) -> undefined
                (L.Cast rd rdType rs rsType) -> undefined
                (L.ArrayRef rd (Left var) idx siz) -> undefined
                (L.ArrayRef rd (Right rs) idx siz) -> undefined
                (L.Val rd (L.Constant literal) -> pushLiteral literal >>= setAddr rd
                (L.Branch rs blkTrue blkFalse) -> do
                  ns>>=loadTo (OReg rs) (A.TReg 0)
                  bne (A.TReg 0) A.ZERO (blockLabel' blkTrue)
                  j (blockLabel' blkFalse)

                (L.Jump bid) -> j $ blockLabel' bid
                (L.Return valueM) -> j $ blockLabel "RETURN"
              return $ instCount + 1
