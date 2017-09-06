{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------
-- |
-- Module    : Text.JSON.Canonical.Types
-- Copyright : (c) Duncan Coutts 2015, 2017
--
module Text.JSON.Canonical.Types
  ( JSValue(..)
  , Int54(..)
  ) where

import Control.Arrow (first)
import Data.Bits (Bits)
#if MIN_VERSION_base(4,7,0)
import Data.Bits (FiniteBits)
#endif
import Data.Data (Data)
import Data.Int (Int64)
import Data.Ix (Ix)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Text.Printf (PrintfArg)


data JSValue
    = JSNull
    | JSBool     !Bool
    | JSNum      !Int54
    | JSString   String
    | JSArray    [JSValue]
    | JSObject   [(String, JSValue)]
    deriving (Show, Read, Eq, Ord)

-- | 54-bit integer values
--
-- JavaScript can only safely represent numbers between @-(2^53 - 1)@ and
-- @2^53 - 1@.
--
-- TODO: Although we introduce the type here, we don't actually do any bounds
-- checking and just inherit all type class instance from Int64. We should
-- probably define `fromInteger` to do bounds checking, give different instances
-- for type classes such as `Bounded` and `FiniteBits`, etc.
newtype Int54 = Int54 { int54ToInt64 :: Int64 }
  deriving ( Enum
           , Eq
           , Integral
           , Data
           , Num
           , Ord
           , Real
           , Ix
#if MIN_VERSION_base(4,7,0)
           , FiniteBits
#endif
           , Bits
           , Storable
           , PrintfArg
           , Typeable
           )

instance Bounded Int54 where
  maxBound = Int54 (  2^(53 :: Int) - 1)
  minBound = Int54 (-(2^(53 :: Int) - 1))

instance Show Int54 where
  show = show . int54ToInt64

instance Read Int54 where
  readsPrec p = map (first Int54) . readsPrec p

