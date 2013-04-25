{-# LANGUAGE CPP, Rank2Types, TypeFamilies, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TemplateHaskell, DeriveDataTypeable #-}
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

module Numeric.AD.Internal.Tower
    () where

import Control.Monad
import Prelude hiding (all)
import Numeric.AD.Internal.Classes

-- | @Tower@ is an AD 'Mode' that calculates a tangent tower by forward AD, and provides fast 'diffsUU', 'diffsUF'
newtype Tower s a = Tower [a]

type instance Scalar (Tower s a) = a

instance Show a => Show (Tower s a) where
    showsPrec n (Tower as) = showParen (n > 10) $ showString "Tower " . showList as

-- Local combinators

tangents :: Tower s a -> Tower s a
tangents (Tower []) = Tower []
tangents (Tower (_:xs)) = Tower xs
{-# INLINE tangents #-}

truncated :: Tower s a -> Bool
truncated (Tower []) = True
truncated _ = False
{-# INLINE truncated #-}

bundle :: a -> Tower s a -> Tower s a
bundle a (Tower as) = Tower (a:as)
{-# INLINE bundle #-}

instance Num a => Primal (Tower s a) where
    primal (Tower (x:_)) = x
    primal _ = 0

instance (Num a) => Mode (Tower s a) where
    auto a = Tower [a]

    Tower [] <+> bs = bs
    as <+> Tower [] = as
    Tower (a:as) <+> Tower (b:bs) = Tower (c:cs)
        where
            c = a + b
            Tower cs = Tower as <+> Tower bs

lift1 f df b   = bundle (f (primal b)) (tangents b * df b)

lift1_ f df b = a where
    a = bundle (f (primal b)) (tangents b * df a b)

binary f dadb dadc b c = bundle (f (primal b) (primal c)) (tangents b * dadb + tangents c * dadc)

lift2 f df b c =
  bundle (f (primal b) (primal c)) tana
  where (dadb, dadc) = df b c
        tanb = tangents b
        tanc = tangents c
        tana = case (truncated tanb, truncated tanc) of
          (False, False) -> tanb * dadb + tanc * dadc
          (True, False) -> tanc * dadc
          (False, True) -> tanb * dadb
          (True, True) -> Tower []

instance Num a => Num (Tower s a) where
    fromInteger 0  = Tower []
    fromInteger n = auto (fromInteger n)
    (+)          = (<+>) -- binary (+) one one
    (-)          = binary (-) (auto 1) (auto (-1)) -- TODO: <-> ? as it is, this might be pretty bad for Tower
    (*)          = lift2 (*) (\x y -> (y, x))
    negate       = lift1 negate (const (auto (-1)))
    abs          = lift1 abs signum
    signum a     = lift1 signum (const $ Tower []) a
instance Fractional a => Fractional (Tower s a) where
    fromRational 0 = Tower []
    fromRational r = auto (fromRational r)
    x / y        = x * recip y
    recip        = lift1_ recip (const . negate . join (*))
instance Floating a => Floating (Tower s a) where
    pi       = auto pi
    exp      = lift1_ exp const
    log      = lift1 log recip
    logBase x y = log y / log x
    sqrt     = lift1_ sqrt (\z _ -> recip (auto 2 * z))
    (**)     = undefined
    --x ** y
    --   | isKnownZero y     = 1
    --   | isKnownConstant y, y' <- primal y = lift1 (** y') ((y'*) . (**(y'-1))) x
    --   | otherwise         = lift2_ (**) (\z xi yi -> (yi * z / xi, z * log1 xi)) x y
    sin      = lift1 sin cos
    cos      = lift1 cos $ negate . sin
    tan      = lift1 tan $ recip . join (*) . cos
    asin     = lift1 asin $ \x -> recip (sqrt (auto 1 - join (*) x))
    acos     = lift1 acos $ \x -> negate (recip (sqrt (one - join (*) x)))
    atan     = lift1 atan $ \x -> recip (one + join (*) x)
    sinh     = lift1 sinh cosh
    cosh     = lift1 cosh sinh
    tanh     = lift1 tanh $ recip . join (*) . cosh
    asinh    = lift1 asinh $ \x -> recip (sqrt (one + join (*) x))
    acosh    = lift1 acosh $ \x -> recip (sqrt (join (*) x - one))
    atanh    = lift1 atanh $ \x -> recip (one - join (*) x)
