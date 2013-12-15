{-# LANGUAGE DeriveFunctor #-}

module Language.BLang.Data (
  ASTAttr(..),
  Line(..),
  spanLine,
  beginOfLine,
  Assoc(..),
  emptyA,
  lookupA,
  (!),
  insertA,
  deleteA,
  memberA,
  notMemberA
) where

data ASTAttr a = Node a [ASTAttr a]
               deriving (Show, Functor)

data Line = NoLineInfo
          | Line { lineNo :: Integer, colNo :: Integer, lineStr :: String }

type Assoc key val = [(key, val)]

emptyA :: Assoc key val
emptyA = []

lookupA :: Ord key => key -> Assoc key val -> Maybe val
lookupA = lookup

(!) :: (Ord key, Show key) => Assoc key val -> key -> val
assoc ! k = case lookupA k assoc of
  Nothing -> error ("lookupA': key '" ++ show k ++ "' not exist")
  Just v -> v

insertA :: Ord key => key -> val -> Assoc key val -> Assoc key val
insertA k v = ((k,v):)

deleteA :: Ord key => key -> Assoc key val -> Assoc key val
deleteA key = filter ((/= key) . fst)

memberA :: Ord key => key -> Assoc key val -> Bool
memberA k assoc = k `elem` map fst assoc

notMemberA :: Ord key => key -> Assoc key val -> Bool
notMemberA = (not .) . memberA

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

beginOfLine :: Integral a => a -> String -> Line
beginOfLine line str = makeLine line 1 str
