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

        * Assignment/variable reference is actually kind of strange now.

            a = 5 + b;
                Const (Reg 0 TInt) (IntLiteral 5)
                Load (Reg 1 TInt) (Left "b")
                Let (Reg 2 TInt) Plus (Reg 0 TInt) (Reg 1 TInt)
                Store (Left "a") (Reg 2 TInt)

            p[0] = p[1] + 3.0
                ??? something is missing
 -}

data Prog v = Prog { progFuncs :: Assoc String (Func v)
                   , progVars :: Assoc String v }

data Func v = Func { funcName :: String
                   , funcVars :: Assoc String v -- **all** local variables, parameters and global variables
                   , funcFrameSize :: Integer
                   , funcCode :: Assoc String [AST] }
                   -- dictionary of blocks, {name:code}. Exactly one block should called "entry" which has no predecessors.

type Reg = Int

data Value = LiteralVal Literal
           | Reg Reg
           deriving (Show)

data AST = Phi Reg [Reg] -- register merging, can only appear in the very begin of every block
         | Call Reg String [Value]
         | Let Reg Operator Value Value  -- let dst = src1 `op` src2
         | Load Reg (Either String Reg)  -- reg <- *(string|reg)
         | Store (Either String Reg) Reg -- *(string|reg) <- reg
         | Var Reg String                -- reg <- &string
         | Cast { castDst :: Reg
                , castDstType :: S.Type
                , castSrc :: Reg
                , castSrcType :: S.Type } -- dst :: t1 <- src :: t2
         | ArrayRef { refDst  :: Reg
                    , refBase :: Reg
                    , refIdx  :: Reg
                    , refSize :: Integer }
         | Constant Reg Value -- constants
         | Branch Reg String String -- branch %res true-branch false-branch
         | Jump String
         | Return (Maybe Value)
         deriving (Show)
