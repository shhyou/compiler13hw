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
