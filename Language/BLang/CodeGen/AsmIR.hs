module Language.BLang.CodeGen.AsmIR where

import Data.List (intercalate)

import Language.BLang.Data

type DataVar = (String, Data)

data Prog v = Prog { progData :: [DataVar]
                   , progFuncs :: [Func v]
                   , progVars :: Assoc String v }

data Func v = Func { funcName :: String
                   , funcVars :: Assoc String v
                   , funcFrameSize :: Integer
                   , funcEnter :: [Inst]
                   , funcCode :: [Inst]
                   , funcData :: [DataVar] }

data Reg = ZERO   -- orange
         | VReg Int
         | AReg Int
         | TReg Int
         | SReg Int
         | FReg Int
         | GP
         | SP
         | FP
         | RA
         deriving (Eq)

data Op = LA | LI
        | LW | SW
        | ADD | SUB | MUL | DIV | MFHI | MFLO | SLT | XOR | SNE
        | BEQ | BNE | J | JAL | JR
        | MTC1 | MFC1
        | LS | SS
        | MOVES
        | CVTWS | CVTSW
        | ADDS | SUBS | MULS | DIVS
        | SYSCALL -- rtype

data Inst = RType { rOp :: Op, rArgs :: [Reg] }
          | IType { iOp :: Op, iDst :: Reg, iSrc :: Reg, iImm :: Either String Int }
          | JType { jOp :: Op, jImm :: String }
          | Label String

data Data = Text String
          | Word [Int]
          | Float [Double]
          | Space Int

instance Show Reg where
  show (VReg x) = "$v" ++ show x
  show (AReg x) = "$a" ++ show x
  show (TReg x) = "$t" ++ show x
  show (SReg x) = "$s" ++ show x
  show (FReg x) = "$f" ++ show x
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
  show SLT = "slt"
  show XOR = "xor"
  show SNE = "sne"
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


showInst x ys = intercalate " ," (x:ys)

showImm _ (Left str) = str
showImm roff (Right coff) = show coff ++ "(" ++ show roff ++ ")"

instance Show Inst where
  show (RType op args) = showInst (show op) (map show args)

  show (IType LA dst _ (Left imm)) = showInst "la" [show dst, show imm]
  show (IType LI dst _ (Right imm)) = showInst "li" [show dst, show imm]
  show (IType LW dst s imm) = showInst "lw" [show dst, showImm s imm]
  show (IType SW dst s imm) = showInst "sw" [show dst, showImm s imm]
  show (IType LS dst s imm) = showInst "ls" [show dst, showImm s imm]
  show (IType SS dst s imm) = showInst "ss" [show dst, showImm s imm]
  show (IType BEQ s t (Left imm)) = showInst "beq" [show s, show t, show imm]
  show (IType BNE s t (Left imm)) = showInst "bne" [show s, show t, show imm]
  show (IType op d s imm) = showInst (show op ++ "i") [show d, showImm s imm]

  show (JType J imm) = "j " ++ show imm
  show (JType JAL imm) = "jal " ++ show imm

  show (Label lbl) = lbl ++ ":"

instance Show Data where
  show (Text str) = ".asciiz " ++ show str
  show (Word ints) = ".word " ++ intercalate ", " $ map show ints
  show (Float dbls) = ".float " ++ intercalate ", " $ map show dbl
  show (Space spc) = ".space " ++ show spc

showData :: [DataVar] -> String
showData = foldl folder ".data:\n"
  where folder acc (name, data') = acc ++ name ++ ": " ++ show data' ++ "\n"

showInsts :: [Inst] -> String
showInsts = concat . map fmt
  where
    fmt lbl@(Label _) = show lbl ++ "\n"
    fmt els = "    " ++ show els ++ "\n"

instance Show (Prog v) where
  show (Prog data' funcs vars) = foldl folder (showData data') funcs
    where folder acc func = acc ++ show func

instance Show (Func v) where
  show func = ".text\n" ++
              showInsts (Label (funcName func) : funcEnter func ++ funcCode func) ++
              showData (funcData func)
