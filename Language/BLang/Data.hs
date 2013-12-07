{-# LANGUAGE DeriveFunctor #-}

module Language.BLang.Data (
  ASTAttr(..),
  Line(..),
  spanLine,
  makeLine
) where

data ASTAttr a = Node a [ASTAttr a]
               deriving (Show, Functor)

data Line = NoLineInfo
          | Line { lineNo :: Integer, colNo :: Integer, lineStr :: String }

instance Show Line where
  show NoLineInfo = "(unknown line)"
  show (Line line col str) =
    "line " ++ show line ++ ", column " ++ show col ++ ":\n" ++
    str ++ (if null str || last str == '\n' then [] else "\n")

spanLine :: String -> String
spanLine []            = []
spanLine ('\r':'\n':_) = "\r\n"
spanLine (c:_)
  | c `elem` "\r\n"    = [c]
spanLine (x:xs)        = x:spanLine xs

makeLine :: (Integral a, Integral b) => a -> b -> String -> Line
makeLine line col content = Line { lineNo = toInteger line,
                                   colNo = toInteger col,
                                   lineStr = spanLine content }

