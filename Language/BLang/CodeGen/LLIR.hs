module Language.BLang.CodeGen.LLIR (
  Operator(..),
  Literal(..),
  S.Type(..)
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

data Prog v = Prog { progInit :: AST -- global variable initialize
                   , progFuncs :: [Func v]
                   , progVars :: Assoc String v }

data Func v = Func { funcName :: String
                   , funcVars :: Assoc String v -- **all** local variables, parameters and global variables
                   , funcFrameSize :: Integer
                   , funcCode :: [AST] }

data Reg = Reg Int S.Type    -- temporaries
         deriving (Show)

data AST = If Reg AST (Maybe AST)
         | For { forInit :: [AST]
               , forCond :: Maybe AST -- if i have got it right, we need only look at the last assigned `reg`
               , forCode :: AST }
         | Whlie { whileCond :: Maybe AST -- if i have got it right, we need only look at the last assigned `reg`
                 , whileCode :: AST }
         | Call Reg String [Reg]
         | Let Reg Operator Reg Reg -- let dst = src1 `op` src2
         | Deref { drefDst :: Reg
                 , drefArr :: Reg
                 , drefIdx :: Reg
                 , drefSize :: Integer }
         | Cast { castDst :: Reg
                , castDstType :: S.Type
                , castSrc :: Reg
                , castSrcType :: S.Type } -- dst :: t1 <- src :: t2
         | Load  Reg (Either String Reg) -- variables
         | Store (Either String Reg) Reg -- variables
         | Const Reg Literal -- constants
         | Return (Maybe Reg)
         deriving (Show)
