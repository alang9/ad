{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- {-# OPTIONS_HADDOCK hide, prune #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Numeric.AD.Tower.Internal
-- Copyright   : (c) Edward Kmett 2010
-- License     : BSD3
-- Maintainer  : ekmett@gmail.com
-- Stability   : experimental
-- Portability : GHC only
--
-----------------------------------------------------------------------------

module Numeric.AD.Internal.Tower () where

-- | @Tower@ is an AD 'Mode' that calculates a tangent tower by forward AD, and provides fast 'diffsUU', 'diffsUF'

type family Scalar t

newtype Tower s a = Tower [a]

type instance Scalar (Tower s a) = a

class (Num (Scalar t), Num t) => Mode t where
    auto  :: Scalar t -> t
    (<+>) :: t -> t -> t

instance (Num a) => Mode (Tower s a) where
    auto a = Tower [a]

    Tower [] <+> bs = bs
    as <+> Tower [] = as
    Tower (a:as) <+> Tower (b:bs) = Tower (c:cs)
        where
            c = a + b
            Tower cs = Tower as <+> Tower bs


instance Num a => Num (Tower s a) where
