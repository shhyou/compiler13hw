module Language.BLang.CodeGen.MIPSTrans where

import qualified Language.BLang.Semantic.AST as S
import qualified Language.BLang.CodeGen.LLIR as L
import qualified Language.BLang.CodeGen.AsmIR as A

import Language.BLang.Semantic.Type (tySize)
import Language.BLang.Data


data Obj = OVar String
         | OReg Int
         | OTxt String
         deriving (Show)

instance Ord Obj where
  (OVar va) <= (OVar vb) = va <= vb
  (OReg ra) <= (OReg vb) = ra <= rb
  (OTxt ta) <= (OTxt tb) = ta <= tb
  (OVar _) <= _ = True
  (OReg _) <= _ = True
  (OTxt _) <= _ = True
  _ <= _ = False

data Addr = AReg A.Reg
          | AData String
          | AMem Int A.Reg
          | AVoid
          | AMadoka
          deriving (Show)

type NameSpace = Assoc Obj Addr


newtype Foo a = Foo (a, [A.Inst], [A.DataVar], NameSpace) deriving (Show)

runFoo :: Foo a -> (a, [A.Inst], [A.DataVar])
runFoo (Foo (x, y, z)) = (x, reverse y, reverse z)

runFoo' (Foo (_, y, z) = (reverse y, reverse z)

rinst op args = Foo ((), [A.RType op args], [])
iinst op rd rs imm = Foo ((), [A.IType op rd rs imm], [])
jinst op imm = Foo ((), [A.JType op imm], [])
label lbl = Foo ((), [A.Label lbl], [])

linsts xs = Foo ((), xs, [])

dtext txt = Foo ((), [], [A.Text txt])
dword int = Foo ((), [], [A.Word [int]])
dfloat dbl = Foo ((), [], [A.Float [dbl]])

instance Functor (Foo a) where
  fmap f (Foo (x, y, z, w)) = Foo (f x, y, z, w)

instance Monad (Foo a) where
  return x = Foo (x, [], [], emptyA)
  (Foo (x, y, z, w)) >>= f =
    let (Foo (x', y', z', w')) = f x
    in Foo (x', y' ++ y, z' ++ z, w' `unionA` w)


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

    newData = dataVars ("GLOBAL_VAR_" ++) globalVars
    newFuncs = undefined
    newVars = undefined

    transFunc :: L.Func L.VarInfo -> A.Func v
    transFunc (L.Func fname fargs fvars fentry fcode) =
      A.Func fname newFvars newFrameSize newFuncEnter newFuncCode newFuncData
      where
        -- fname :: String
        -- fargs :: [(String, S.Type)]
        -- fvars :: Assoc String L.VarInfo
        -- fentry :: Int <- should be L.Label
        -- fcode :: Assoc L.Label [L.AST]
        -- newFvars :: Assoc String v  <-- what?
        -- newFrameSize :: Int
        -- newFuncEnter :: [A.Inst]
        -- newFuncCode :: [A.Inst]
        -- newFuncData :: [A.DataVar]

        funcLabel = (fname ++ "_" ++)
        blockLabel = funcLabel . ("BLK_" ++)
        blockLabel' = blockLabel . show

        newFrameSize = sum $ fmap (tySize . L.varType) fargs
        newFuncData = dataVars (funcLabel . ("VAR_" ++)) fvars

        newFuncEnter = fst . runFoo' $ do
          iinst A.SW A.RA A.SP (Right -4)
          iinst A.SW A.FP A.SP (Right -8)
          rinst A.ADD A.FP A.SP A.ZERO
          mapM_ (\x -> iinst A.SW (A.SReg x) A.SP (Right -12 - 4*x)) [0..7]
          iinst A.SUB A.SP A.SP (Right (40 + newFrameSize))
          jinst A.J (Left . blockLabel $ show fentry)

        -- newReg, findReg, freeReg, showLiteral, getVal

        transBlock :: [L.AST] -> ([A.Inst], [A.DataVar])
        transBlock = runFoo' . foldl transInst (return ())
          where
            transInst :: Foo NameSpace -> L.AST -> Foo NameSpace
            transInst nsInFoo last = do
              ns <- nsInFoo
              case last of
                (L.Phi rd srcs) -> error "Can I not implement this?"
                (L.Call rd "write") -> undefined
                (L.Call rd "read" []) -> do
                  iinst A.LW (A.VReg 0) A.ZERO (Right 5)
                  rinst A.SYSCALL []
                  rd' <- alloc rd
                  rinst A.ADD [rd', A.VReg 0, A.ZERO]

                (L.Call rd "fread" []) -> do
                  iinst A.LW (A.VReg 0) A.ZERO (Right 5)
                  rinst A.SYSCALL []
                  rd' <- alloc rd
                  rinst A.MOVES [rd', A.FReg 0]

                (L.Call rd fname args) -> undefined
                (L.Let rd L.Negate [val]) -> undefined
                (L.Let rd op vals) -> undefined
                (L.Load rd (Left var)) -> undefined
                (L.Load rd (Right reg)) -> undefined
                (L.Store (Left var) rs) -> undefined
                (L.Store (Right rd) rs) -> undefined
                (L.Cast rd rdType rs rsType) -> undefined
                (L.ArrayRef rd (Left var) idx siz) -> undefined
                (L.ArrayRef rd (Right rs) idx siz) -> undefined
                (L.Val rd val) -> undefined
                (L.Branch rs blkTrue blkFalse) -> undefined
                (L.Jump bid) -> jinst A.J (blockLabel' bid)
                (L.Return valueM) -> jinst A.J (blockLabel "RETURN")
