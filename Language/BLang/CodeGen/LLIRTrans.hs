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

data Cont m = Sym String | Fun (L.Value -> m [L.AST])

llirTrans :: S.Prog S.Type -> L.Prog L.VarInfo
llirTrans (S.Prog decls funcs) = undefined

cps :: MonadState St m => [S.AST S.Type] -> Cont m -> m [L.AST]
cps ((S.Block sym codes):cs) k = undefined
cps ((S.Expr ty _ rator rands):cs) k = undefined
cps ((S.ImplicitCast ty' ty e):cs) k = undefined
cps ((S.For _ forinit forcond foriter forcode):cs) k = undefined
cps ((S.While _ whcond whcode):cs) k = undefined
cps ((S.Ap ty _ fn args):cs) k = undefined
cps ((S.If _ con th Nothing):cs) k = undefined
cps ((S.If _ con th (Just el)):cs) k = undefined
cps ((S.Return _ Nothing):cs) k = undefined
cps ((S.Return _ (Just val)):cs) k = undefined
cps ((S.Identifier ty _ name):cs) k = undefined
cps ((S.LiteralVal _ lit):cs) k = undefined
cps ((S.ArrayRef ty _ ref idx):cs) k = undefined
cps (S.Nop:cs) k = cps cs k
