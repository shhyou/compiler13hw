module Language.BLang.CodeGen.AsmIR where

import Data.Char (toLower)

import Language.BLang.Data

data Prog v = Prog { progData :: [(String, Data)]
                   , progFuncs :: [Func v]
                   , progVars :: Assoc String v }

data Func v = Func { funcName :: String
                   , funcVars :: Assoc String v
                   , funcFrameSize :: Integer
                   , funcEnter :: [Inst]
                   , funcCode :: [Inst]
                   , funcLeave :: [Inst]
                   , funcData :: [(String, Data)] }

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
        | BEQ | BNE | J | JAL | JR
        | MTC1 | MFC1
        | LS | SS
        | CVTWS | CVTSW
        | ADDS | SUBS | MULS | DIVS
        | SYSCALL -- rtype

data Inst = RType { rOp :: Op, rDst :: Reg, rSrc1 :: Reg, rSrc2 :: Reg }
          | IType { iOp :: Op, iDst :: Reg, iSrc :: Reg, iImm :: Either String Int }
          | JType { jOp :: Op, jImm :: String }
          | Label String

data Data = Text String
          | Word Int
          | Float Double

instance Show Reg where
  show (VReg x) = "$v" ++ show x
  show (AReg x) = "$a" ++ show x
  show (TReg x) = "$t" ++ show x
  show (SReg x) = "$s" ++ show x
  show ZERO = "$zero"
  show GP = "$gp"
  show SP = "$sp"
  show FP = "$fp"
  show RA = "$ra"

instance Show Op where
  show LA = "la"
  show LI = "li"
  show LW = "lw"
  show SW = "sw"
  show ADD = "add"
  show SUB = "sub"
  show MUL = "mul"
  show DIV = "div"
  show MFHI = "mfhi"
  show MFLO = "mflo"
  show BEQ = "beq"
  show BNE = "bne"
  show J = "j"
  show JAL = "jal"
  show JR = "jr"
  show SYSCALL = "syscall"
  show MTC1 = "mtc1"
  show MFC1 = "mfc1"
  show LS = "l.s"
  show SS = "s.s"
  show CVTWS = "cvt.w.s"
  show CVTSW = "cvt.s.w"
  show ADDS = "add.s"
  show SUBS = "sub.s"
  show MULS = "mul.s"
  show DIVS = "div.s"


showInst x ys = x ++ " " ++ intercalate ", " ys

instance Show Inst where
  show (RType MFHI dst _ _) = "mfhi " ++ show dst
  show (RType MFLO dst _ _) = "mflo " ++ show dst
  show (RType JR dst _ _) = "jr " ++ show dst
  show (RType SYSCALL _ _ _) = "syscall"
  show (RType op d s t) = showInst (show op) [show d, show s, show t]

  show (IType LA dst _ imm) = showInst "la" [show dst, show imm]
  show (IType LI dst _ imm) = showInst "li" [show dst, show imm]
  show (IType LW dst roff coff) = showInst "lw" [show dst, show coff ++ "(" ++ show roff ++ ")"]
  show (IType SW dst roff coff) = showInst "sw" [show dst, show coff ++ "(" ++ show roff ++ ")"]
  show (IType BEQ s t imm) = showInst "beq" [show s, show t, show imm]
  show (IType BNE s t imm) = showInst "bne" [show s, show t, show imm]
  show (IType op d s imm) = showInst (show op ++ "i") [show d, show s, show imm]

  show (JType J imm) = "j " ++ show imm
  show (JType JAL imm) = "jal " ++ show imm

  show (Label lbl) = lbl ++ ":"

instance Show Data where
  show (Text str) = ".asciiz \"" ++ str ++ "\""
  show (Word int) = ".word " ++ show int
  show (Float dbl) = ".float " ++ show dbl

showData :: [(String, Data)] -> String
showData xs = if null xs then "" else foldl folder ".data:\n" xs
  where folder acc (name, data') = acc ++ name ++ ": " ++ show data' ++ "\n"

instance Show (Prog v) where
  show (Prog data' funcs vars) = foldl folder (show data') funcs
    where folder acc func = acc ++ show func

instance Show (Func v) where
  show func = undefined
