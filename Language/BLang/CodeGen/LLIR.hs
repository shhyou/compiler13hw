module Language.BLang.CodeGen.LLIR (
  Operator(..),
  P.Literal(..),
  S.Type(..),
  Prog(..),
  Func(..),
  VarInfo(..),
  Reg(..),
  RegInfo(..),
  Label(..),
  Value(..),
  AST(..),
  fromParserOp
) where

import Prelude hiding (LT, EQ, GT)

import Data.List (intercalate, sortBy)

import Language.BLang.Data
import qualified Language.BLang.FrontEnd.Parser as P (Operator(..), Literal(..))
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

data Operator = Plus | Minus | Times | Divide | Negate
              | LT   | GT    | LEQ   | GEQ    | EQ | NEQ
              | LNot | SetNZ -- Let dst SetNZ [src]; dst <- src? 1 : 0
              deriving (Show, Eq)

fromParserOp :: P.Operator -> Operator
fromParserOp op = case lookup op mapping of
  Just op' -> op'
  Nothing  -> error $ "fromParserOp: cannot map parser operator '" ++ show op ++ "'"
  where mapping = [(P.Plus, Plus), (P.Minus, Minus), (P.Times, Times), (P.Divide, Divide), (P.Negate, Negate),
                   (P.LT, LT), (P.GT, GT), (P.LEQ, LEQ), (P.GEQ, GEQ), (P.EQ, EQ), (P.NEQ, NEQ), (P.LNot, LNot)]

data Prog v = Prog { progVars :: Assoc String v
                   , progFuncs :: Assoc String (Func v)
                   , progRegs :: Assoc Reg S.Type }

data Func v = Func { funcName :: String
                   , funcArgs :: [(String, S.Type)] -- an *ordered* set, for function parameters
                   , funcVars :: Assoc String v -- **all** local variables
                   , funcEntry :: Label
                   , funcCode :: Assoc Label [AST] }
                   -- dictionary of blocks, {name:code}. Exactly one block, the entry, should has no predecessors.
instance Show v => Show (Func v) where
  show (Func name args vars entry codes) =
    "fun " ++ name ++ "(" ++ intercalate "," (map show args) ++ "):" ++ show entry ++ "\n"
    ++ " " ++ show vars ++ "\n"
    ++ (flip concatMap (sortBy ((. fst) . compare . fst) $ toListA codes) $ \(lbl, codes) ->
      "  " ++ show lbl ++ ":\n" ++ (flip concatMap codes $ \c -> "    " ++ show c ++ ";\n"))

data VarInfo = VarInfo { varName :: String, varType :: S.Type }
instance Show VarInfo where
  show (VarInfo name ty) = name ++ ":" ++ show ty

newtype Reg = TempReg Int deriving (Eq, Ord)
instance Show Reg where
  show (TempReg n) = '%':show n
data RegInfo = RegInfo { regType :: S.Type, regFunc :: String, regAssignedAt :: String } deriving (Show)

newtype Label = BlockLabel Int deriving (Eq, Ord)
instance Show Label where
  show (BlockLabel n) = 'L':show n

data Value = Constant P.Literal
           | Var String                -- &string
           | Reg Reg
           deriving (Show)

data AST = Phi Reg [(Label, Value)] -- register merging, can only appear in the very begin of every block
         | Call Reg String [Value] -- var <- f(vs); for `void` functions, bindings could be eliminated in latter phases
         | Let Reg Operator [Value]      -- let dst = op [srcs]
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
         | Branch Reg Label Label -- branch %res true-branch false-branch
         | Jump Label -- jump block
         | Return (Maybe Value)

instance Show AST where
  show (Phi reg entry) = show reg ++ " <- phi" ++ show entry
  show (Call reg fn vals) = show reg ++ " <- " ++ fn ++ show vals
  show (Let reg op vals) = show reg ++ " <- " ++ show op ++ show vals
  show (Load reg src) = show reg ++ " <- !" ++ showsPrec 11 src []
  show (Store dst reg) = show dst ++ " := " ++ show reg
  show (Cast dst ty' src ty) = show dst ++ ":" ++ show ty' ++ " <- " ++ show src ++ ":" ++ show ty
  show (ArrayRef dst base idx siz) = show dst ++ " <- " ++ showsPrec 11 base [] ++ "[" ++ showsPrec 11 idx [] ++ "*" ++ show siz ++ "]"
  show (Val reg val) = show reg ++ " <- " ++ show val
  show (Branch reg tru fal) = "branch " ++ show reg ++ " true->" ++ show tru ++ "; false->" ++ show fal
  show (Jump lbl) = "jump " ++ show lbl
  show (Return val) = "ret " ++ showsPrec 11  val []
