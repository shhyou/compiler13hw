module Language.BLang.Semantic.Type where

import Language.BLang.Data (Line(NoLineInfo))
import qualified Language.BLang.FrontEnd.Parser as P (Type(..), ASTStmt(LiteralVal), Literal(IntLiteral))
import qualified Language.BLang.Semantic.AST as S

fromParserType :: P.Type -> S.Type
fromParserType (P.TPtr t) = S.TPtr (fromParserType t)
fromParserType (P.TArray ixs t) = S.TArray (map getIdx ixs) (fromParserType t)
  where getIdx (P.LiteralVal _ (P.IntLiteral n)) = n
        getIdx _ = error "fromParserType: Cannot convert non-literal array dimension"
fromParserType (P.TCustom s) = error ("fromParserType: Cannot convert TCustom type '" ++ s ++ "'")
fromParserType P.TInt = S.TInt
fromParserType P.TFloat = S.TFloat
fromParserType P.TChar = S.TChar
fromParserType P.TVoid = S.TVoid

toParserType :: S.Type -> P.Type
toParserType (S.TPtr t) = P.TPtr (toParserType t)
toParserType (S.TArray ixs t) = P.TArray (map putIdx ixs) (toParserType t)
  where putIdx n = P.LiteralVal NoLineInfo (P.IntLiteral n)
toParserType (S.TArrow ts t) = error "toParserType: Cannot convert TArrow type"
toParserType S.TInt = P.TInt
toParserType S.TFloat = P.TFloat
toParserType S.TChar = P.TChar
toParserType S.TVoid = P.TVoid

-- though `char` should be of integer types too, it is not supported here
tyIsIntType :: S.Type -> Bool
tyIsIntType S.TInt = True
tyIsIntType _      = False

tyIsArithType :: S.Type -> Bool
tyIsArithType S.TFloat = True
tyIsArithType t
  | tyIsIntType t      = True
tyIsArithType _        = False

tyIsScalarType :: S.Type -> Bool
tyIsScalarType (S.TPtr _) = True
tyIsScalarType t
  | tyIsArithType t       = True
tyIsScalarType _          = False

-- n1570 6.3.1.1-2
-- convert integer type whose rank <= int/unsigned int to int/unsigned int.
tyIntPromotion :: S.Type -> S.Type
tyIntPromotion S.TChar = S.TInt
tyIntPromotion t = t -- for TInt, TFloat

-- n1570 6.3.1.8
-- should call integer promotion when there are more integer types
-- `double`, `char` is not supported now
tyUsualArithConv :: S.Type -> S.Type -> S.Type
tyUsualArithConv _        S.TFloat = S.TFloat
tyUsualArithConv S.TFloat _        = S.TFloat
tyUsualArithConv t1       t2
  | tyIntPromotion t1 ==
    tyIntPromotion t2              = tyIntPromotion t1
tyUsualArithConv _        _        = error "tyUsualArithConv: unsupported type"

-- n1570 6.3.2.1-3, array to pointer decay
tyArrayDecay :: S.Type -> S.Type
tyArrayDecay (S.TArray [_] t) = S.TPtr t
tyArrayDecay (S.TArray ixs t) = S.TPtr (S.TArray (tail ixs) t)
tyArrayDecay t = t
