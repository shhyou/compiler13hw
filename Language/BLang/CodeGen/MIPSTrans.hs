module Language.BLang.CodeGen.MIPSTrans where

import qualified Language.BLang.Semantic.AST as S
import qualified Language.BLang.CodeGen.LLIR as L
import qualified Language.BLang.CodeGen.AsmIR as AIR

import Language.BLang.Data


-- Memo:
-- 1. L.reg -> AIR.Reg, L.reg -> Int/Float
-- 2. AIR.Reg :: RegType -> RegSpace -> (AIR.Reg, RegSpace)
-- 3.


-- TODO
toAIR :: L.Prog v -> AIR.Prog v
toAIR (L.Prog funcs vars regs) = AIR.Prog data' funcs' vars
  where
    dataFolder acc new = new : acc
    data' = reverse $ foldl dataFolder [] vars'

    funcFolder acc new = doit new : acc
    funcs' = reverse $ foldl funcFolder [] funcs'
-- .data: global variables
-- .text: function body ++ .data: local constants for each function

-- function begin:
--   store ra, fp
--   set new ra, fp
--   save used $s series

data Block = Block { blkId :: Int,
                     lstmts :: [L.AST],
                     astmts :: [AIR.Inst]
                     preds :: [Int] } -- predecessors

transFunc :: L.Func v -> (Block, Assoc Int Block)
transFunc lfunc = (entry, shit)
  where
    label str = "_" ++ funcName lfunc ++ "_" ++ str
    blkLabel str = label $ "BLK_" ++ str
    blkLabel' idx = blkLabel (show idx)

    frameSize = undefined -- size of local vars

    entry = AIR.IType AIR.SW AIR.RA AIR.SP (Right -4) :
            AIR.IType AIR.SW AIR.FP AIR.SP (Right -8) :
            AIR.RType AIR.ADD AIR.FP AIR.SP AIR.ZERO :
            map (\x -> AIR.IType AIR.SW (AIR.SReg x) AIR.SP (Right (-12 - 4 * x))) [0..7] ++
            AIR.IType AIR.SUB AIR.SP AIR.SP (Right (40 + frameSize)) :
            AIR.JType AIR.J (Left (blkLabel' (funcEntry lfunc)))

    transBlock bid blk = Block blk (foldl folder ([], info...) blk) []
      where
        folder (acc, info...) = undefined
        -- astmts, reginfo , text block,

    doit (L.Phi rd srcs) = error "plz dont use this"

  --data Literal = IntLiteral Integer
  --              FloatLiteral Double
  --              StringLiteral String

    doit (L.Call rd "write" [Constant literal]) =
      -- save (show literal content) to text
      -- save $t0~..
      -- call write
      undefined

    doit (L.Call rd "write" [Var var]) = undefined
    doit (L.Call rd "write" [Reg reg]) = undefined
    doit (L.Call rd "read" []) =
      AIR.IType AIR.LI (AIR.VReg 0) (Right 5) :
      AIR.RType AIR.SYSCALL AIR.ZERO AIR.ZERO AIR.ZERO :
      undefined
    doit (L.Call rd "fread" []) = undefined
    doit (L.Call rd fname args) = undefined
    -- store $t0~
    -- push arguments in reverse order
    -- jal
    -- reverse changes

    doit (L.Let op xs) = undefined

    doit (L.Load rd (Left var)) = undefined
    doit (L.Load rd (Right reg)) = undefined
    doit (L.Store (Left var) rs) = undefined
    doit (L.Store (Right rd) rs) = undefined
    doit (L.Cast rd rdType rs rsType) = undefined -- reg. alloc
    doit (L.ArrayRef rd (Left var) idx siz) = undefined -- reg. alloc
    doit (L.ArrayRef rd (Right rs) idx siz) = undefined -- reg. alloc
    doit (L.Val rd val) = undefined -- reg. alloc
    doit (L.Branch rs blkTrue blkFalse) =  -- reg. lookup
      AIR.IType AIR.BNE ({- address of rs -}) AIR.ZERO (Left (blkLabel' blkTrue)) :
      AIR.JType AIR.J (blkLabel' blkFalse)

    doit (L.Jump bid) = [AIR.JType AIR.J (blkLabel' bid)]
    doit (L.Return valueM) = [AIR.JType AIR.J (label "RETURN")]
