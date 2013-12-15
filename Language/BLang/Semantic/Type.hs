module Language.BLang.Semantic.Type where

import qualified Language.BLang.Semantic.AST as S

tyUsualArithConv :: S.Type -> S.Type -> S.Type
tyUsualArithConv = undefined

tyIntPromote :: S.Type -> S.Type -> S.Type
tyIntPromote = undefined

-- n1570 6.3.2.1-3, array to pointer decay
tyArrayDecay :: S.Type -> S.Type
tyArrayDecay (S.TArray [_] t) = S.TPtr t
tyArrayDecay (S.TArray ixs t) = S.TPtr (S.TArray (tail ixs) t)
tyArrayDecay t = t
