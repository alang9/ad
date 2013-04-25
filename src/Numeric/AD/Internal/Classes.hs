{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures, Rank2Types, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, PatternGuards, CPP #-}
{-# LANGUAGE FlexibleContexts, FunctionalDependencies, UndecidableInstances, GeneralizedNewtypeDeriving, TemplateHaskell #-}
-- {-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Internal.Classes
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-----------------------------------------------------------------------------

module Numeric.AD.Internal.Classes
    (
    -- * AD modes
      Mode(..)
    , one
    -- * Automatically Deriving AD
    , Primal(..)
    , Iso(..)
    , Scalar
    ) where

type family Scalar t

class Iso a b where
    iso :: f a -> f b
    osi :: f b -> f a

instance Iso a a where
    iso = id
    osi = id

class (Num (Scalar t), Num t) => Mode t where
    auto  :: Scalar t -> t
    (<+>) :: t -> t -> t

one :: Mode t => t
one = auto 1
{-# INLINE one #-}

-- | 'Primal' is used by 'deriveMode' but is not exposed
-- via the 'Mode' class to prevent its abuse by end users
-- via the AD data type.
--
-- It provides direct access to the result, stripped of its derivative information,
-- but this is unsafe in general as (auto . primal) would discard derivative
-- information. The end user is protected from accidentally using this function
-- by the universal quantification on the various combinators we expose.

class Primal t where
    primal :: t -> Scalar t
