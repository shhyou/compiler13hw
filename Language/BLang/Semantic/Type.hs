module Language.BLang.Semantic.Type where

import qualified Language.BLang.FrontEnd.Parser as P (Type(..), ASTStmt(LiteralVal), Literal(IntLiteral))
import qualified Language.BLang.Semantic.AST as S

fromParserType :: P.Type -> S.Type
fromParserType (P.TPtr t) = S.TPtr (fromParserType t)
fromParserType (P.TArray ixs t) = S.TArray (map getIdx ixs) (fromParserType t)
  where getIdx (P.LiteralVal (P.IntLiteral n)) = n
        getIdx _ = error "fromParserType: Cannot convert non-literal array dimension"
fromParserType (P.TCustom s) = error ("fromParserType: Cannot convert TCustom type '" ++ s ++ "'")
fromParserType P.TInt = S.TInt
fromParserType P.TFloat = S.TFloat
fromParserType P.TChar = S.TChar
fromParserType P.TVoid = S.TVoid

toParserType :: S.Type -> P.Type
toParserType (S.TPtr t) = P.TPtr (toParserType t)
toParserType (S.TArray ixs t) = P.TArray (map putIdx ixs) (toParserType t)
  where putIdx n = P.LiteralVal (P.IntLiteral n)
toParserType (S.TArrow ts t) = error "toParserType: Cannot convert TArrow type"
toParserType S.TInt = P.TInt
toParserType S.TFloat = P.TFloat
toParserType S.TChar = P.TChar
toParserType S.TVoid = P.TVoid

-- though `char` should be of integer types too, it is not supported here
tyIntType :: S.Type -> Bool
tyIntType S.TInt = True
tyIntType _      = False

tyArithType :: S.Type -> Bool
tyArithType S.TFloat = True
tyArithType t
  | tyIntType t      = True
tyArithType _        = False

tyScalarType :: S.Type -> Bool
tyScalarType (S.TPtr _) = True
tyScalarType t
  | tyArithType t       = True
tyScalarType _          = False

-- should call integer promotion when there are more integer types
-- `char` is not supported now :)
tyUsualArithConv :: S.Type -> S.Type -> S.Type
tyUsualArithConv S.TInt   S.TInt   = S.TInt
tyUsualArithConv S.TInt   S.TFloat = S.TFloat
tyUsualArithConv S.TFloat S.TInt   = S.TFloat
tyUsualArithConv S.TFloat S.TFloat = S.TFloat
tyUsualArithConv _        _        = error "tyUsualArithConv: unsupported type"

-- n1570 6.3.2.1-3, array to pointer decay
tyArrayDecay :: S.Type -> S.Type
tyArrayDecay (S.TArray [_] t) = S.TPtr t
tyArrayDecay (S.TArray ixs t) = S.TPtr (S.TArray (tail ixs) t)
tyArrayDecay t = t
