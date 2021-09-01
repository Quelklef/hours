module Hours.Gar (class Gar, initial, execute, simulate) where

import Prelude

import Type.Proxy (Proxy(..))
import Data.Foldable (class Foldable, foldl)

class Gar m a s where
  initial :: Proxy m -> Proxy a -> s
  execute :: a -> (s -> m s)

simulate :: forall m f a s. Monad m => Foldable f => Gar m a s => f a -> m s
simulate actions = foldl (\s a -> s >>= execute a) (pure init) actions
  where init = initial (Proxy :: Proxy m) (Proxy :: Proxy a)
