{-# LANGUAGE FlexibleContexts, BangPatterns, MagicHash #-}

module Language.BLang.Miscellaneous where

import qualified GHC.Exts as Exts
import qualified GHC.Integer.Logarithms as Logs

import Control.Monad (liftM2, liftM3)
import Control.Monad.State
import Control.Monad.Reader
import Data.Bits ((.&.))

import Language.BLang.Data (Assoc, unionA, emptyA)

-- CAUTION: n must be strictly positive
dirtyLog2 :: Integer -> Int
dirtyLog2 !n = Exts.I# (Logs.integerLog2# n)

is2pow :: Integer -> Bool
is2pow n = n > 0 && n == (n .&. (-n))

wrapList :: a -> [a]
wrapList x = [x]

mapsnd :: Monad m => (b -> m c) -> (a, b) -> m (a, c)
mapsnd f (a, t) = liftM2 (,) (return a) (f t)

map2nd :: Monad m => (b -> m d) -> (a, b, c) -> m (a, d, c)
map2nd f (a, t, c) = liftM3 (,,) (return a) (f t) (return c)

first f (a, b) = (f a, b)
second f (a, b) = (a, f b)

first3 f (a, b, c) = (f a, b, c)
second3 f (a, b, c) = (a, f b, c)
third3 f (a, b, c) = (a, b, f c)

maybeM :: Monad m => Maybe a -> (a -> m b) -> m (Maybe b)
maybeM (Just a) f = liftM Just (f a)
maybeM Nothing  _ = return Nothing
