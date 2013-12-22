module Language.BLang.CodeGen.LLIR (
  Operator(..),
  Literal(..),
  S.Type(..),
  Prog(..),
  Func(..),
  Reg(..),
  AST(..)
) where

import Language.BLang.Data
import Language.BLang.FrontEnd.Parser (Operator(..), Literal(..))
import qualified Language.BLang.Semantic.AST as S

{- TODO:
    There are several things need to be cared upon transforming to LLIR:
        * Desugar variabel initialization; put it in function codes.

        * For loop iteration shall be merged into `forCode`.
          Actually, I'm not sure whether we still need `For`.

        * Assignment/variable reference is actually kind of weird now.
            a = 5 + b;
                Load 0 (Left "b")
                Let 1 Add (Constant (IntLiteral 5)) (Reg 0)
                Store (Left "a") 1

            float p[100], q[2][3];
            q[0][1] = p[1] + 3.0
                ArrayRef 0 (Left "p") (Constant (IntLiteral 1)) 4
                Load 1 (Right 0)
                Let 2 Add (Reg 1) (Constant (FloatLiteral 3.0))
                ArrayRef 3 (Left "q") (Constant (IntLiteral 0)) 12
                ArrayRef 4 (Right 3) (Constant (IntLiteral 1)) 4
                Store (Right 4) 2

        * In short, `Var`/`Left name`/`ArrayRef` gives l-values. We need
          `Load`/`Store` to access the saved value.

          Standard ML has made references explicit, so is LLIR.
 -}

data Prog v = Prog { progFuncs :: Assoc String (Func v)
                   , progVars :: Assoc String v
                   , progRegs :: Assoc Int S.Type }

data Func v = Func { funcName :: String
                   , funcVars :: Assoc String v -- **all** local variables, parameters and global variables
                   , funcFrameSize :: Integer
                   , funcCode :: Assoc String [AST] }
                   -- dictionary of blocks, {name:code}. Exactly one block should called "entry" which has no predecessors.

type Reg = Int

data Value = Constant Literal
           | Var String                -- &string
           | Reg Reg
           deriving (Show)

data AST = Phi Reg [(String, Reg)] -- register merging, can only appear in the very begin of every block
         | Call Reg String [Value]
         | Let Reg Operator Value Value  -- let dst = src1 `op` src2
         | Load Reg (Either String Reg)  -- reg <- *(string|reg)
         | Store (Either String Reg) Reg -- *(string|reg) <- reg
         | Cast { castDst :: Reg
                , castDstType :: S.Type
                , castSrc :: Reg
                , castSrcType :: S.Type } -- dst :: t1 <- src :: t2
         | ArrayRef { refDst  :: Reg
                    , refBase :: Either String Reg
                    , refIdx  :: Value
                    , refSize :: Integer }
         | Val Reg Value -- reg <- values
         | Branch Reg String String -- branch %res true-branch false-branch
         | Jump String
         | Return (Maybe Value)
         deriving (Show)
