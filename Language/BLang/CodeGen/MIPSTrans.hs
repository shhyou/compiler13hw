-- module for transforming semantic IR into an ANF-inspired IR
module Language.BLang.CodeGen.LLIRTrans where

import qualified Language.BLang.Semantic.AST as S
import qualified Language.BLang.CodeGen.LLIR as L
import qualified Language.BLang.CodeGen.AsmIR as AIR

import Language.BLang.Data


-- Memo:
-- * Float consts and strings should be stored in data area
-- * read, fread, write should be specially dealt with
-- * Maybe phi function should be dealed with in its block's preceder
-- * Optimizations implemented only if we have spare time
--    1. Detect which registers don't need to be stored to stack (using flow graph)
--    2. Assign $sx registers to those who are used after function call


-- load $t0, coff($roff)
data Address = Address { coff :: Int,
                         roff :: AIR.Reg }

instance Show Address where
  show (Address coff' roff') = show coff' ++ "(" ++ show roff' ++ ")"

-- v is TBD
toAIR :: L.Prog v -> AIR.Prog v
toAIR (Prog funcs vars regs) = undefined
-- .data: global variables
-- .text: function body ++ .data: local constants for each function

-- function begin:
--   store ra, fp
--   set new ra, fp
--   save used $s series

doit (Phi _ _) = error "plz dont use this"
doit (Call rd fname args) = undefined
-- push arguments in reverse order
-- jal
-- reverse changes

doit (Let rd op rs rt) = undefined

doit (Load rd (Left var)) = undefined
doit (Load rd (Right reg)) = undefined
doit (Store (Left var) rs) = undefined
doit (Store (Right rd) rs) = undefined
doit (Cast rd rdType rs rsType) = undefined
doit (ArrayRef rd (Left var) idx siz) = undefined
doit (ArrayRef rd (Right rs) idx siz) = undefined
doit (Val rd val) = undefined
doit (Branch rs blkTrue blkFalse) = undefined
doit (Jump blk) = undefined
doit (Return valueM) = undefined -- jump to <function_name>_return
