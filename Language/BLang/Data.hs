{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}

module Language.BLang.Data (
  ASTAttr(..),

  Line(..),
  spanLine,
  beginOfLine,

  Assoc,
  emptyA,
  lookupA,
  (!),
  insertA,
  unionA,
  deleteA,
  filterA,
  memberA,
  notMemberA
) where

import Control.Arrow (second)
import Control.Applicative ((<$>))
import qualified Data.Foldable as F
import qualified Data.Traversable as T

data ASTAttr a = Node a [ASTAttr a]
               deriving (Show, Functor)

data Line = NoLineInfo
          | Line { lineNo :: Integer, colNo :: Integer, lineStr :: String }

newtype Assoc key val = Assoc { unAssoc :: [(key, val)] } deriving (Show)

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

instance Functor (Assoc k) where
  fmap f (Assoc xs) = Assoc $ fmap (second f) xs

instance F.Foldable (Assoc k) where
  foldMap f (Assoc xs) = F.foldMap f (map snd xs)

instance T.Traversable (Assoc k) where
  traverse f (Assoc xs) = Assoc <$> (zip (map fst xs) <$> T.traverse f (map snd xs))

emptyA :: Assoc key val
emptyA = Assoc []

lookupA :: Ord key => key -> Assoc key val -> Maybe val
lookupA = (. unAssoc) . lookup

(!) :: (Ord key, Show key) => Assoc key val -> key -> val
assoc ! k = case lookupA k assoc of
  Nothing -> error ("lookupA': key '" ++ show k ++ "' not exist")
  Just v -> v

insertA :: Ord key => key -> val -> Assoc key val -> Assoc key val
insertA k v = Assoc . ((k,v):) . unAssoc

unionA :: Ord key => Assoc key val -> Assoc key val -> Assoc key val
unionA = (Assoc .) . (. unAssoc) . (++) . unAssoc

filterA :: Ord key => (val -> Bool) -> Assoc key val -> Assoc key val
filterA = (Assoc .) . (. unAssoc) . (filter $) . (. snd)

deleteA :: Ord key => key -> Assoc key val -> Assoc key val
deleteA key = Assoc . filter ((/= key) . fst) . unAssoc

memberA :: Ord key => key -> Assoc key val -> Bool
memberA k (Assoc assoc) = k `elem` map fst assoc

notMemberA :: Ord key => key -> Assoc key val -> Bool
notMemberA = (not .) . memberA
