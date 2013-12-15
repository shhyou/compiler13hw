module Language.BLang.Miscellaneous where

import Control.Monad (liftM2, liftM3)

wrapList :: a -> [a]
wrapList x = [x]

mapsnd :: Monad m => (b -> m c) -> (a, b) -> m (a, c)
mapsnd f (a, t) = liftM2 (,) (return a) (f t)

map2nd :: Monad m => (b -> m d) -> (a, b, c) -> m (a, d, c)
map2nd f (a, t, c) = liftM3 (,,) (return a) (f t) (return c)
