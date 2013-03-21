{-# LANGUAGE RankNTypes #-}
module Numeric.AD.Mode.Forward.Double
    ( jacobian
    ) where

import Data.Traversable (Traversable)
import Numeric.AD.Types
import Numeric.AD.Internal.Forward.Double

jacobian :: (Traversable f, Traversable g) => (forall s. f (AD ForwardDouble s Double) -> g (AD ForwardDouble s Double)) -> f Double -> g (f Double)
jacobian f as = transposeWith (const id) t p
    where
        (p, t) = bind' (fmap tangent . f) as
{-# INLINE jacobian #-}
