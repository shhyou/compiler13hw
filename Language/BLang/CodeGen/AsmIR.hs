module Language.BLang.CodeGen.AsmIR where

import Data.Char (toLower)

import Language.BLang.Data

data Prog v = Prog { progInit :: [Inst]
                   , progFuncs :: [Func v]
                   , progVars :: Assoc String v }

data Func v = Func { funcName :: String
                   , funcVars :: Assoc String v
                   , funcFrameSize :: Integer
                   , funcEnter :: [Inst]
                   , funcCode :: [Inst]
                   , funcLeave :: [Inst] }

-- Note that some expressions might need more than 8 regs.
data Reg = ZERO   -- orange
         | VReg Int
         | AReg Int
         | TReg Int
         | SReg Int
         | GP
         | SP
         | FP
         | RA

data Op = LA | LI
        | LW | SW
        | ADD | SUB | MUL | DIV | MFHI | MFLO
        | BEQ | BNE | J | JAL | JR  -- do we need JR?
        deriving (show)

data Inst = RType { rOp :: Op, rDst :: Reg, rSrc1 :: Reg, rSrc2 :: Reg }
          | IType { iOp :: Op, iDst :: Reg, iSrc :: Reg, iImm :: Either String Int }
          | JType { jOp :: Op, jImm :: String }
          | SYSCALL


instance Show Reg where
  show ZERO = "$zero"
  show (VReg x) = "$v" ++ show x
  show (AReg x) = "$a" ++ show x
  show (TReg x) = "$t" ++ show x
  show (SReg x) = "$s" ++ show x
  show GP = "$gp"
  show SP = "$sp"
  show FP = "$fp"
  show RA = "$ra"

showInst x ys = x ++ " " ++ intercalate ", " ys

toStr :: Op -> String
toStr = map toLower . show

instance Show Inst where
  show (RType MFHI dst _ _) = "mfhi " ++ show dst
  show (RType MFLO dst _ _) = "mflo " ++ show dst
  show (RType op d s t) = showInst (toStr op) [show d, show s, show t]

  show (IType LA dst _ imm) = showInst "la" [show dst, show imm]
  show (IType LI dst _ imm) = showInst "li" [show dst, show imm]
  show (IType LW dst roff coff) = showInst "lw" [show dst, show coff ++ "(" ++ show roff ++ ")"]
  show (IType SW dst roff coff) = showInst "sw" [show dst, show coff ++ "(" ++ show roff ++ ")"]
  show (IType BEQ s t imm) = showInst "beq" [show s, show t, show imm]
  show (IType BNE s t imm) = showInst "bne" [show s, show t, show imm]
  show (IType op d s imm) = showInst (toStr op ++ "i") [show d, show s, show imm]

  show (JType J imm) = "j " ++ show imm
  show (JType JAL imm) = "jal " ++ show imm
