module Language.BLang.Semantic.NormalizeAST where

import Data.Maybe (isJust)

import Language.BLang.Data
import Language.BLang.Semantic.Type
import qualified Language.BLang.Semantic.AST as S

normalize :: S.Prog S.Var -> S.Prog S.Type
normalize = evalConst . removeVarInit . alphaConv

-- alpha conversion, so that no shadowing would occur
-- 'main' function should remain unchanged
alphaConv :: S.Prog S.Var -> S.Prog S.Var
alphaConv = undefined

-- remove global/local variable initialization code and remove 
removeVarInit :: S.Prog S.Var -> S.Prog S.Type
removeVarInit (S.Prog decls funcs) = S.Prog decls' funcs'
  where decls' = undefined
        funcs' = undefined
        main   = funcs ! "main"

remVarFunc :: S.FuncDecl S.Var -> S.FuncDecl S.Type
remVarFunc f@(S.FuncDecl _ _ code) = f { S.funcCode = remVarAST code }

remVarAST :: S.AST S.Var -> S.AST S.Type
remVarAST (S.Block symtbl stmts) =
  undefined
remVarAST (S.Expr ty line op stmts) =
  S.Expr ty line op (map remVarAST stmts)
remVarAST (S.ImplicitCast ty' ty stmt) =
  S.ImplicitCast ty' ty (remVarAST stmt)
remVarAST (S.For line forinit forcond foriter forcode) =
  S.For line (map remVarAST forinit) (map remVarAST forcond) (map remVarAST foriter) (remVarAST forcode)
remVarAST (S.While line whcond whcode) =
  S.While line (map remVarAST whcond) (remVarAST whcode)
remVarAST (S.Ap ty line fn args) =
  S.Ap ty line (remVarAST fn) (map remVarAST args)
remVarAST (S.If line con th el) =
  S.If line (remVarAST con) (remVarAST th) (fmap remVarAST el)
remVarAST (S.Return line val) =
  S.Return line (fmap remVarAST val)
remVarAST (S.Identifier ty line name) =
  S.Identifier ty line name
remVarAST (S.LiteralVal line lit) =
  S.LiteralVal line lit
remVarAST (S.ArrayRef ty line base idx) =
  S.ArrayRef ty line (remVarAST base) (remVarAST idx)
remVarAST S.Nop =
  S.Nop

-- eval constant expressions to simplify LLIR transformation
evalConst :: S.Prog S.Type -> S.Prog S.Type
evalConst = undefined
