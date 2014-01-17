
{- actually not quite Sethi-Ullman. register count is merely an estimation -}
module Language.BLang.BackEnd.SethiUllman (
  seull
) where

import Language.BLang.Data

import qualified Language.BLang.Semantic.AST as S

seull :: S.Prog S.Type -> S.Prog S.Type
seull (S.Prog decls funcs) = S.Prog decls (fmap seullFunc funcs)

seullFunc :: S.FuncDecl S.Type -> S.FuncDecl S.Type
seullFunc (S.FuncDecl ty args vars codes) = S.FuncDecl ty args vars (map seullAST' codes)

seullAST' = snd . seullAST

xchgableOperators :: Assoc S.Operator S.Operator
xchgableOperators = fromListA
  [(S.Plus, S.Plus), (S.Times, S.Times), (S.LT, S.GT), (S.GT, S.LT),
   (S.LEQ, S.GEQ), (S.GEQ, S.LEQ), (S.EQ, S.EQ), (S.NEQ, S.NEQ)]

seullAST :: S.AST S.Type -> (Int, S.AST S.Type)
seullAST (S.For forinit forcond foriter forcode) =
  (0, S.For (map seullAST' forinit) (map seullAST' forcond) (map seullAST' foriter) (map seullAST' forcode))
seullAST (S.While whcond whcode) =
  (0, S.While (map seullAST' whcond) (map seullAST' whcode))
seullAST (S.If con th el) =
  (0, S.If (seullAST' con) (map seullAST' th) (fmap (map seullAST') el))
seullAST (S.Return val) =
  (0, S.Return $ fmap seullAST' val)
seullAST (S.Expr ty rator [rand]) =
  (reg, S.Expr ty rator [rand'])
  where (reg, rand') = seullAST rand
seullAST (S.Expr ty rator [rand1, rand2]) | rator `memberA` xchgableOperators =
  if reg1 >= reg2
    then (max reg1 (1 + reg2), S.Expr ty rator [rand1', rand2'])
    else (max reg2 (1 + reg1), S.Expr ty rator' [rand2', rand1'])
  where (reg1, rand1') = seullAST rand1
        (reg2, rand2') = seullAST rand2
        rator' = xchgableOperators ! rator
seullAST (S.Expr ty rator rands) =
  (maximum $ zipWith (+) regs [0..], S.Expr ty rator rands')
  where (regs, rands') = unzip $ map seullAST rands
seullAST (S.ImplicitCast ty' ty e) =
  (reg, S.ImplicitCast ty' ty e')
  where (reg, e') = seullAST e
seullAST (S.Ap ty fn args) =
  (maximum regs, S.Ap ty fn args')
  where (regs, args') = unzip $ map seullAST args
seullAST (S.ArrayRef ty base idx) =
  (max reg1 (reg2+1), S.ArrayRef ty base' idx')
  where (reg1, base') = seullAST base
        (reg2, idx')  = seullAST idx
seullAST s = (1, s) -- Identifier, LiteralVal
