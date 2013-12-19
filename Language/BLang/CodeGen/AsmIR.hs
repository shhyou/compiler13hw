module Language.BLang.CodeGen.AsmIR where

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
        | SYSCALL

data Inst = RType Op Reg Reg Reg
          | IType Op Reg Reg Int
          | JType Op Int
