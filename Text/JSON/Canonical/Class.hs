{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif

--------------------------------------------------------------------
-- |
-- Module    : Text.JSON.Canonical.Class
-- Copyright : (c) Edsko de Vries, Duncan Coutts 2015
--
--
-- Type classes and utilities for converting to and from 'JSValue'.
--
module Text.JSON.Canonical.Class (
    -- * Type classes
    ToJSON(..)
  , FromJSON(..)
  , ToObjectKey(..)
  , FromObjectKey(..)
  , ReportSchemaErrors(..)
  , Expected
  , Got
  , expectedButGotValue
    -- * Utility
  , fromJSObject
  , fromJSField
  , fromJSOptField
  , mkObject
  ) where

import Text.JSON.Canonical.Types

import Control.Monad (foldM, liftM)
import Data.Maybe (catMaybes)
import Data.Map (Map)
import qualified Data.Map as Map

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative (Applicative, (<$>), (<*>))
#endif


--import Hackage.Security.Util.Path

{-------------------------------------------------------------------------------
  ToJSON and FromJSON classes

  We parameterize over the monad here to avoid mutual module dependencies.
-------------------------------------------------------------------------------}

class ToJSON m a where
  toJSON :: a -> m JSValue

class FromJSON m a where
  fromJSON :: JSValue -> m a

-- | Used in the 'ToJSON' instance for 'Map'
class ToObjectKey m a where
  toObjectKey :: a -> m String

-- | Used in the 'FromJSON' instance for 'Map'
class FromObjectKey m a where
  fromObjectKey :: String -> m (Maybe a)

-- | Monads in which we can report schema errors
class (Applicative m, Monad m) => ReportSchemaErrors m where
  expected :: Expected -> Maybe Got -> m a

type Expected = String
type Got      = String

expectedButGotValue :: ReportSchemaErrors m => Expected -> JSValue -> m a
expectedButGotValue descr val = expected descr (Just (describeValue val))
  where
    describeValue :: JSValue -> String
    describeValue (JSNull    ) = "null"
    describeValue (JSBool   _) = "bool"
    describeValue (JSNum    _) = "num"
    describeValue (JSString _) = "string"
    describeValue (JSArray  _) = "array"
    describeValue (JSObject _) = "object"

unknownField :: ReportSchemaErrors m => String -> m a
unknownField field = expected ("field " ++ show field) Nothing

{-------------------------------------------------------------------------------
  ToObjectKey and FromObjectKey instances
-------------------------------------------------------------------------------}

instance Monad m => ToObjectKey m String where
  toObjectKey = return

instance Monad m => FromObjectKey m String where
  fromObjectKey = return . Just

{-------------------------------------------------------------------------------
  ToJSON and FromJSON instances
-------------------------------------------------------------------------------}

instance Monad m => ToJSON m JSValue where
  toJSON = return

instance Monad m => FromJSON m JSValue where
  fromJSON = return

instance Monad m => ToJSON m String where
  toJSON = return . JSString

instance ReportSchemaErrors m => FromJSON m String where
  fromJSON (JSString str) = return str
  fromJSON val            = expectedButGotValue "string" val

instance Monad m => ToJSON m Int54 where
  toJSON = return . JSNum

instance ReportSchemaErrors m => FromJSON m Int54 where
  fromJSON (JSNum i) = return i
  fromJSON val       = expectedButGotValue "int" val

instance
#if __GLASGOW_HASKELL__ >= 710
  {-# OVERLAPPABLE #-}
#endif
    (Monad m, ToJSON m a) => ToJSON m [a] where
  toJSON = liftM JSArray . mapM' toJSON

instance
#if __GLASGOW_HASKELL__ >= 710
  {-# OVERLAPPABLE #-}
#endif
    (ReportSchemaErrors m, FromJSON m a) => FromJSON m [a] where
  fromJSON (JSArray as) = mapM' fromJSON as
  fromJSON val          = expectedButGotValue "array" val


instance ( Monad m
         , ToObjectKey m k
         , ToJSON m a
         ) => ToJSON m (Map k a) where
  toJSON = liftM JSObject . mapM' aux . Map.toList
    where
      aux :: (k, a) -> m (String, JSValue)
      aux (k, a) = (,) <$> toObjectKey k <*> toJSON a

instance ( ReportSchemaErrors m
         , Ord k
         , FromObjectKey m k
         , FromJSON m a
         ) => FromJSON m (Map k a) where
  fromJSON enc = do
      obj <- fromJSObject enc
      Map.fromList . catMaybes <$> mapM_reverse aux obj
    where
      aux :: (String, JSValue) -> m (Maybe (k, a))
      aux (k, a) = knownKeys <$> fromObjectKey k <*> fromJSON a
      knownKeys :: Maybe k -> a -> Maybe (k, a)
      knownKeys Nothing  _ = Nothing
      knownKeys (Just k) a = Just (k, a)

{-------------------------------------------------------------------------------
  Utility
-------------------------------------------------------------------------------}

fromJSObject :: ReportSchemaErrors m => JSValue -> m [(String, JSValue)]
fromJSObject (JSObject obj) = return obj
fromJSObject val            = expectedButGotValue "object" val

-- | Extract a field from a JSON object
fromJSField :: (ReportSchemaErrors m, FromJSON m a)
            => JSValue -> String -> m a
fromJSField val nm = do
    obj <- fromJSObject val
    case lookup nm obj of
      Just fld -> fromJSON fld
      Nothing  -> unknownField nm

fromJSOptField :: (ReportSchemaErrors m, FromJSON m a)
               => JSValue -> String -> m (Maybe a)
fromJSOptField val nm = do
    obj <- fromJSObject val
    case lookup nm obj of
      Just fld -> Just <$> fromJSON fld
      Nothing  -> return Nothing

mkObject :: forall m. Monad m => [(String, m JSValue)] -> m JSValue
mkObject = liftM JSObject . sequenceFields
  where
    sequenceFields :: [(String, m JSValue)] -> m [(String, JSValue)]
    sequenceFields []               = return []
    sequenceFields ((fld,val):flds) = do val' <- val
                                         flds' <- sequenceFields flds
                                         return ((fld,val'):flds')

-- Avoid stack overflow on large lists
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f = fmap reverse . mapM_reverse f

-- For when we don't care about order, can avoid the reverse
mapM_reverse :: Monad m => (a -> m b) -> [a] -> m [b]
mapM_reverse f = foldM (\xs a -> fmap (:xs) (f a)) []

