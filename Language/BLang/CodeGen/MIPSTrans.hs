module Language.BLang.CodeGen.MIPSTrans where

import qualified Language.BLang.Semantic.AST as S
import qualified Language.BLang.CodeGen.LLIR as L
import qualified Language.BLang.CodeGen.AsmIR as AIR

import Control.Monad.Writer

import Language.BLang.Data

type WInsts = Writer [AIR.Inst]


-- TODO
toAIR :: L.Prog L.VarInfo -> AIR.Prog v
toAIR (L.Prog funcs vars regs) = AIR.Prog data' funcs' vars
  where
    dataFolder acc new = new : acc
    data' = reverse $ foldl dataFolder [] vars'

    funcFolder acc new = doit new : acc
    funcs' = reverse $ foldl funcFolder [] funcs'

-- .data: global variables
-- .text: function body ++ .data: local constants for each function


data Addr = AVar String
          | AReg Int
          | ATxt String

instance Ord Addr where
  (AVar va) <= (AVar vb) = va <= vb
  (AReg ra) <= (AReg rb) = ra <= rb
  (ATxt ta) <= (ATxt tb) = ta <= tb
  (AVar _) <= _ = True
  (AReg _) <= _ = True
  (ATxt _) <= _ = True
  _ <= _ = False

data Block = Block { blkId :: Int,
                     lstmts :: [L.AST],
                     astmts :: [AIR.Inst]
                     preds :: [Int] } -- predecessors

data RegCtnt = RCVar String
             | RCReg L.Reg
             | RCData AIR.Data
             | RCVoid

type NameSpace = Assoc Addr RegCtnt


transFunc :: L.Func L.VarInfo -> (Block, Assoc Int Block, [(String, AIR.Data)])
transFunc lfunc = (snd . runWriter $ entry, shit, moreshit)
  where
    label = ("_" ++ funcName lfunc ++ "_" ++)
    blkLabel = label . ("BLK_" ++)
    blkLabel' = blkLabel . show

    frameSize = undefined -- size of local vars


    tellR = (((tell . (:[]) .) .) .) . AIR.RType
    tellI = (((tell . (:[]) .) .) .) . AIR.IType
    tellJ = (((tell . (:[]) .) .) .) . AIR.JType
    tellL = (tell . (:[]) .) . AIR.Label


    entry = do
      tellI AIR.SW AIR.RA AIR.SP (Right -4)
      tellI AIR.SW AIR.FP AIR.SP (Right -8)
      tellR AIR.ADD AIR.FP AIR.SP AIR.ZERO
      tell $ map (\x -> AIR.IType AIR.SW (AIR.SReg x) AIR.SP (Right (-12 - 4 * x))) [0..7]
      tellI AIR.SUB AIR.SP AIR.SP (Right (40 + frameSize))
      tellJ AIR.J (Left (blkLabel' (funcEntry lfunc)))


    newReg :: L.Reg -> NameSpace -> Writer [AIR.Inst] (AIR.Reg, NameSpace)
    newReg lreg regs = do
      -- if no free reg: spill
      return undefined

    findReg :: L.Reg -> NameSpace -> Write [AIR.Inst] (AIR.Reg, NameSpace)
    findReg lreg regs = do
      -- if reg is in memory : getReg >> load >> ret
      return undefined

    freeReg :: L.Reg -> NameSpace -> Writer [AIR.Inst] NameSpace
    freeReg = undefined

    showLiteral (IntLiteral int) = show int
    showLiteral (FloatLiteral dbl) = show dbl
    showLiteral (StringLiteral str) = show str -- intended

    getVal :: L.Value -> NameSpace -> Writer [AIR.Inst] (AIR.Reg, NameSpace)
    getVal (L.Reg reg) regSpace = undefined
    getVal (L.Var var) regSpace = undefined
    getVal (L.Constant literal) regSpace = undefined



    transBlock bid blk = Block blk (snd . runWriter $ foldl folder ([], info...) blk) []
      where
        folder (acc, info...) = undefined
        -- astmts, reginfo , text block, shit

    callFunc insts = do
      -- store $ts
      -- insts
      -- restore $ts
      return undefined

    syscall sid = do
      tellI AIR.LI (AIR.VReg 0) (Right sid)
      tellR AIR.SYSCALL AIR.ZERO AIR.ZERO AIR.ZERO

    doit (L.Phi rd srcs) = error "plz dont use this"

    doit (L.Call rd "write" [Constant (IntLiteral int)]) = do
      tellI AIR.LI (AIR.AReg 0) AIR.ZERO (Right int)
      syscall 1

    doit (L.Call rd "write" [Constant (FloatLiteral int)]) = do
      tellI AIR.LI (AIR.FReg 12) AIR.ZERO (Right int)
      syscall 2

    doit (L.Call rd "write" [Constant (StringLiteral int)]) = do
      let lbl = undefined
      tellI AIR.LA (AIR.AReg 0) AIR.ZERO (Left lbl)
      syscall 4

    -- int: 1 @ $a0, float: 2 @ $f12, string: 4
    doit (L.Call rd "write" [Var var]) = undefined
    doit (L.Call rd "write" [Reg reg]) = undefined
    doit (L.Call rd "read" []) = callFunc do
      syscall 5
      -- alloc a reg for rd and move shit

    doit (L.Call rd "fread" []) = undefined -- 6
    doit (L.Call rd fname args) = callFunc do
      -- push arguments in reverse order
      return undefined

    doit (L.Let rd L.Negate [val]) = undefined

    doit (L.Load rd (Left var)) = undefined
    doit (L.Load rd (Right reg)) = undefined
    doit (L.Store (Left var) rs) = undefined
    doit (L.Store (Right rd) rs) = undefined
    doit (L.Cast rd rdType rs rsType) = undefined -- reg. alloc
    doit (L.ArrayRef rd (Left var) idx siz) = undefined -- reg. alloc
    doit (L.ArrayRef rd (Right rs) idx siz) = undefined -- reg. alloc
    doit (L.Val rd val) = undefined -- reg. alloc
    doit (L.Branch rs blkTrue blkFalse) = do -- reg. lookup
      tellI AIR.BNE ({- address of rs -}) AIR.ZERO (Left (blkLabel' blkTrue))
      tellJ AIR.J (blkLabel' blkFalse)

    doit (L.Jump bid) = tellJ AIR.J (blkLabel' bid)
    doit (L.Return valueM) = tellJ AIR.J (label "RETURN")
