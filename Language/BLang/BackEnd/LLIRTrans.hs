{-# LANGUAGE FlexibleContexts, DoRec #-}

-- module for transforming semantic IR into an ANF-inspired IR
module Language.BLang.BackEnd.LLIRTrans where

import qualified Data.Traversable as T (mapM)
import Control.Applicative (Applicative(), (<$>), (<*>))
import Control.Monad (forM)
import Control.Monad.Fix
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Cont

import Language.BLang.Data
import Language.BLang.Miscellaneous

import qualified Language.BLang.Semantic.AST as S
import Language.BLang.Semantic.Type
import qualified Language.BLang.BackEnd.LLIR as L

-- global state
data St = St { getRegCnt :: Int -- next available register number
             , getBlockCnt :: Int -- next available block number
             , getRegTypes :: Assoc L.Reg L.Type -- the type of each register
             , getLocalVars :: Assoc String L.Type -- (current) function's local variables
             , getCurrBlock :: L.Label
             , getExitLabel :: Assoc L.Label L.Label
             , getCodes :: Assoc L.Label [L.AST] } -- existed blocks

updateRegCnt    f st = st { getRegCnt = f (getRegCnt st) }
updateBlockCnt  f st = st { getBlockCnt = f (getBlockCnt st) }
updateRegTypes  f st = st { getRegTypes = f (getRegTypes st) }
setLocalVars vars st = st { getLocalVars = vars }
updateCodes     f st = st { getCodes = f (getCodes st) }
setCurrBlock  lbl st = st { getCurrBlock = lbl }
updateExitLabel f st = st { getExitLabel = f (getExitLabel st) }

