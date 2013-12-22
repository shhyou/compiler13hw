module Language.BLang.Semantic.NormalizeAST where

import qualified Language.BLang.Semantic.AST as S

normalize :: S.Prog S.Var -> S.Prog S.Type
normalize = undefined

removeVarInit :: S.Prog S.Var -> S.Prog S.Type
removeVarInit = undefined

evalConst :: S.Prog S.Type -> S.Prog S.Type
evalConst = undefined
