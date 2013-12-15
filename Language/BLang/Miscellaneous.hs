module Language.BLang.Miscellaneous where

import Control.Monad (liftM2, liftM3)

mapMSnd :: Monad m => (b -> m c) -> (a, b) -> m (a, c)
mapMSnd f (a, t) = liftM2 (,) (return a) (f t)

mapM2nd :: Monad m => (b -> m d) -> (a, b, c) -> m (a, d, c)
mapM2nd f (a, t, c) = liftM3 (,,) (return a) (f t) (return c)
