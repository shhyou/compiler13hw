module Language.BLang.CodeGen.BlockOrder where

import Data.List (sortBy)
import Text.Regex.Posix

import qualified Language.BLang.CodeGen.AsmIR as A

splitAtLabel :: [A.Inst] -> [[A.Inst]]
splitAtLabel [] = [[]]
splitAtLabel [A.Label lbl] | lbl =~ "_BLK_" = [[], [A.Label lbl]]
splitAtLabel (A.Label lbl:xs) | lbl =~ "_BLK_" = []:(A.Label lbl:hd):rest
  where (hd:rest) = splitAtLabel xs
splitAtLabel (x:xs) = (x:hd):rest
  where (hd:rest) = splitAtLabel xs

blockOrder :: A.Prog v -> A.Prog v
blockOrder prog = prog { A.progFuncs = map blockOrderFunc (A.progFuncs prog) }

blockOrderFunc :: A.Func v -> A.Func v
blockOrderFunc func = func { A.funcCode = concat $ sortBy cmpf codes }
  where codes = tail (splitAtLabel (A.funcCode func))
        A.JType A.J lbl0 = last (A.funcEnter func)
        cmpf blk1@(A.Label lbl1:_) blk2@(A.Label lbl2:_) =
          if lbl1 == lbl2 then EQ
          else if lbl1 == lbl0 then LT
          else if lbl2 == lbl0 then GT
          else if lbl1 =~ "_RETURN$" then GT
          else if lbl2 =~ "_RETURN$" then LT
          else let val1 = (read (lbl1 =~ "[0-9]+$" :: String) :: Int)
                   val2 = (read (lbl2 =~ "[0-9]+$" :: String) :: Int)
               in compare val1 val2

jumpElim :: A.Prog v -> A.Prog v
jumpElim prog = prog { A.progFuncs = map jumpElimFunc (A.progFuncs prog) }
  where jumpElimFunc func = func { A.funcEnter = init (A.funcEnter func)
                                 , A.funcCode = jumpElim' (A.funcCode func) }

jumpElim' :: [A.Inst] -> [A.Inst]
jumpElim' ((A.JType A.J lbl):(A.Label lbl'):xs) | lbl == lbl' = 
  (A.Label lbl'):jumpElim' xs
jumpElim' (x:xs) =
  x:jumpElim' xs
jumpElim' [] =
  []
