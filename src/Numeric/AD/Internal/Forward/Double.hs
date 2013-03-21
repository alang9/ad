{-# LANGUAGE BangPatterns, ConstraintKinds, FlexibleContexts, TemplateHaskell, TypeFamilies, UndecidableInstances #-}

module Numeric.AD.Internal.Forward.Double
    ( ForwardDouble(..)
    , tangent
    , bundle
    , bind
    , bind'
    , transposeWith
    ) where

import Language.Haskell.TH
import Control.Applicative
import Data.Traversable (Traversable, mapAccumL)
import Data.Foldable (Foldable, toList)
import Numeric.AD.Internal.Types
import Numeric.AD.Internal.Classes
import Numeric.AD.Internal.Identity

data ForwardDouble a = ForwardDouble !Double Double

type instance Domain ForwardDouble a = a ~ Double

-- | Calculate the 'tangent' using forward mode AD.
tangent :: AD ForwardDouble s Double -> Double
tangent (AD (ForwardDouble _ da)) = da
{-# INLINE tangent #-}

bundle :: Double -> Double -> AD ForwardDouble s Double
bundle a da = AD (ForwardDouble a da)
{-# INLINE bundle #-}

instance Primal ForwardDouble where
    primal (ForwardDouble a _) = a

instance Lifted ForwardDouble => Mode ForwardDouble where
    auto = flip ForwardDouble 0
    zero = ForwardDouble 0 0

    isKnownZero (ForwardDouble 0 0) = True
    isKnownZero _ = False

    isKnownConstant (ForwardDouble _ 0) = True
    isKnownConstant _ = False

    ForwardDouble a da <+> ForwardDouble b db = ForwardDouble (a + b) (da + db)

    x    <**> y      = lift2_ (**) (\z xi yi -> (yi *! z /! xi, z *! log1 xi)) x y

    a *^ ForwardDouble b db = ForwardDouble (a * b) (a * db)

    ForwardDouble a da ^* b = ForwardDouble (a * b) (da * b)

    ForwardDouble a da ^/ b = ForwardDouble (a / b) (da / b)

instance Lifted ForwardDouble => Jacobian ForwardDouble where
    type D ForwardDouble = Id


    unary f (Id dadb) (ForwardDouble b db) = ForwardDouble (f b) (dadb * db)

    lift1 f df (ForwardDouble b db) = ForwardDouble (f b) (dadb * db)
        where
            Id dadb = df (Id b)

    lift1_ f df (ForwardDouble b db) = ForwardDouble a da
        where
            a = f b
            Id da = df (Id a) (Id b) ^* db

    binary f (Id dadb) (Id dadc) (ForwardDouble b db) (ForwardDouble c dc) = ForwardDouble (f b c) $ dadb * db + dc * dadc

    lift2 f df (ForwardDouble b db) (ForwardDouble c dc) = ForwardDouble a da
        where
            a = f b c
            (Id dadb, Id dadc) = df (Id b) (Id c)
            da = dadb * db + dc * dadc

    lift2_ f df (ForwardDouble b db) (ForwardDouble c dc) = ForwardDouble a da
        where
            a = f b c
            (Id dadb, Id dadc) = df (Id a) (Id b) (Id c)
            da = dadb * db + dc * dadc

deriveLifted id $ conT ''ForwardDouble

bind :: (Traversable f) => (f (AD ForwardDouble s Double) -> b) -> f Double -> f b
bind f as = snd $ mapAccumL outer (0 :: Int) as
    where
        outer !i _ = (i + 1, f $ snd $ mapAccumL (inner i) 0 as)
        inner !i !j a = (j + 1, if i == j then bundle a 1 else auto a)

bind' :: (Traversable f) => (f (AD ForwardDouble s Double) -> b) -> f Double -> (b, f b)
bind' f as = dropIx $ mapAccumL outer (0 :: Int, b0) as
    where
        outer (!i, _) _ = let b = f $ snd $ mapAccumL (inner i) (0 :: Int) as in ((i + 1, b), b)
        inner !i !j a = (j + 1, if i == j then bundle a 1 else auto a)
        b0 = f (auto <$> as)
        dropIx ((_,b),bs) = (b,bs)

-- we can't transpose arbitrary traversables, since we can't construct one out of whole cloth, and the outer
-- traversable could be empty. So instead we use one as a 'skeleton'
transposeWith :: (Functor f, Foldable f, Traversable g) => (b -> f a -> c) -> f (g a) -> g b -> g c
transposeWith f as = snd . mapAccumL go xss0
    where
        go xss b = (tail <$> xss, f b (head <$> xss))
        xss0 = toList <$> as

