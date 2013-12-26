{-# LANGUAGE FlexibleContexts #-}

module Language.BLang.CodeGen.LLIRTrans where

import Control.Applicative ((<$>))
import Control.Monad.State
import Control.Monad.Reader

import Language.BLang.Data

import qualified Language.BLang.Semantic.AST as S
import qualified Language.BLang.CodeGen.LLIR as L

-- global state
data St = St { stRegCnt :: Int
             , stBlockCnt :: Int
             , stCurrBlock :: Int
             , stCodes :: Assoc Int [L.AST] }

updateRegCnt   f st = st { stRegCnt = f (stRegCnt st) }
updateBlockCnt f st = st { stBlockCnt = f (stBlockCnt st) }
setCurrBlock   n st = st { stCurrBlock = n }

freshReg :: (MonadState St m, Functor m) => m Int
freshReg = modify (updateRegCnt (+1)) >> stRegCnt <$> get

llirTrans :: S.Prog S.Var -> L.Prog L.VarInfo
llirTrans (S.Prog decls funcs) = undefined

data Cont m = Sym String | Fun (L.Value -> m [L.AST])

cpse :: MonadState St m => S.AST S.Var -> Cont m -> m [L.AST]
cpse (S.Expr ty _ rator rands) k
  | rator /= S.Assign = undefined
cpse (S.ImplicitCast ty' ty e) k = undefined
cpse (S.Ap ty _ fn args) k = undefined
cpse (S.Identifier ty _ name) k = undefined
cpse (S.LiteralVal _ lit) k = undefined
cpse (S.ArrayRef ty _ ref idx) k = undefined

llTransAST :: MonadState St m => [S.AST S.Var] -> m [L.AST]
llTransAST ((S.Block sym codes):cs) = undefined
llTransAST ((S.For _ forinit forcond foriter forcode):cs) = undefined
llTransAST ((S.While _ whcond whcode):cs) = undefined
llTransAST ((S.If _ con th Nothing):cs) = undefined
llTransAST ((S.If _ con th (Just el)):cs) = undefined
llTransAST ((S.Return _ Nothing):cs) = undefined
llTransAST ((S.Return _ (Just val)):cs) = undefined
llTransAST (S.Nop:cs) = llTransAST cs
