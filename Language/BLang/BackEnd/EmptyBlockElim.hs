module Language.BLang.BackEnd.EmptyBlockElim (
  elim
) where

import Language.BLang.Data
import qualified Language.BLang.BackEnd.LLIR as L

elim :: L.Prog v -> L.Prog v
elim prog = prog { L.progFuncs = fmap emptyBlockFunc (L.progFuncs prog) }

emptyBlockFunc :: L.Func v -> L.Func v
emptyBlockFunc func = func { L.funcCode = codes' }
  where codes = L.funcCode func
        codes' = filterA (not. isEmptyBlock) $ fmap (map (inlineBlock emptyBlks)) codes
        emptyBlks = fmap (\[L.Jump lbl] -> lbl) $ filterA isEmptyBlock codes
        isEmptyBlock [L.Jump _] = True
        isEmptyBlock _ = False

inlineBlock :: Assoc L.Label L.Label -> L.AST -> L.AST
inlineBlock env (L.Branch reg tru fal) =
  L.Branch reg (inlineIfExists env tru) (inlineIfExists env fal)
inlineBlock env (L.Jump lbl) =
  L.Jump (inlineIfExists env lbl)
inlineBlock _ inst = inst

inlineIfExists env key = case lookupA key env of
  Just key' -> key'
  Nothing   -> key
