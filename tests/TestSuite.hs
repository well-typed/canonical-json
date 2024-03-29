{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

import           Data.Int
import           Data.List (sortBy, nubBy)
import           Data.Function (on)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Text.JSON.Canonical

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative (Applicative(..), (<$>))
#endif

import qualified Data.Aeson as Aeson (Value (..), eitherDecode)
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as KeyMap (fromList)
#else
import qualified Data.HashMap.Strict as KeyMap (fromList)
#endif
import           Data.String (IsString, fromString)
import qualified Data.Map            as Map
import qualified Data.Vector         as V  (fromList)

import           Test.QuickCheck
import           Test.Tasty.QuickCheck (testProperty)
import           Test.Tasty (TestTree, defaultMain, testGroup)


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Canonical JSON" [
          testProperty "prop_roundtrip_canonical" prop_roundtrip_canonical
        , testProperty "prop_roundtrip_pretty"    prop_roundtrip_pretty
        , testProperty "prop_canonical_pretty"    prop_canonical_pretty
        , testProperty "prop_aeson_canonical"     prop_aeson_canonical
        , testGroup "prop_toJSON_fromJSON" [
            testProperty "String" prop_toJSON_fromJSON_String
          , testProperty "Int54"  prop_toJSON_fromJSON_Int54
          , testProperty "[]"     prop_toJSON_fromJSON_List
          , testProperty "Map"    prop_toJSON_fromJSON_Map
          ]
        , testGroup "no stack overflow" [
            testProperty "[]"  unit_large_List
          , testProperty "Map" unit_large_Map
          ]
        ]


prop_roundtrip_canonical,
  prop_roundtrip_pretty,
  prop_canonical_pretty,
  prop_aeson_canonical
  :: JSValue -> Bool

prop_roundtrip_canonical jsval =
    parseCanonicalJSON (renderCanonicalJSON jsval) == Right (canonicalise jsval)

prop_roundtrip_pretty jsval =
    parseCanonicalJSON (BS.pack (prettyCanonicalJSON jsval)) == Right jsval

prop_canonical_pretty jsval =
    parseCanonicalJSON (renderCanonicalJSON jsval) ==
    fmap canonicalise (parseCanonicalJSON (BS.pack (prettyCanonicalJSON jsval)))

prop_aeson_canonical jsval =
    Aeson.eitherDecode (renderCanonicalJSON jsval') == Right (toAeson jsval')
  where
    jsval' = omitNonPrintableChars jsval

prop_toJSON_fromJSON :: (Monad m, ToJSON m a, FromJSON m a, Eq a) => a -> m Bool
prop_toJSON_fromJSON x =
    toJSON x >>= fromJSON >>= \x' -> return (x' == x)

prop_toJSON_fromJSON_String :: JSString -> Property
prop_toJSON_fromJSON_String = runM . prop_toJSON_fromJSON

prop_toJSON_fromJSON_Int54 :: Int54 -> Property
prop_toJSON_fromJSON_Int54 = runM . prop_toJSON_fromJSON

prop_toJSON_fromJSON_List :: [JSString] -> Property
prop_toJSON_fromJSON_List = runM . prop_toJSON_fromJSON

prop_toJSON_fromJSON_Map :: Map.Map JSString JSString -> Property
prop_toJSON_fromJSON_Map = runM . prop_toJSON_fromJSON

unit_large_List :: Property
unit_large_List = runM (prop_toJSON_fromJSON large)
  where
    large = replicate 10000 (42 :: Int54)

unit_large_Map :: Property
unit_large_Map  = runM (prop_toJSON_fromJSON large)
  where
    large = Map.fromList [ (show n, 42 :: Int54) | n <- [0.. 10000 :: Int] ]


newtype M a = M (Maybe a)
  deriving (Functor, Applicative, Monad)

runM :: Testable prop => M prop -> Property
runM (M Nothing)     = property False
runM (M (Just prop)) = property prop

instance ReportSchemaErrors M where
  expected _ _ = M Nothing

canonicalise :: JSValue -> JSValue
canonicalise v@JSNull        = v
canonicalise v@(JSBool    _) = v
canonicalise v@(JSNum     _) = v
canonicalise v@(JSString  _) = v
canonicalise   (JSArray  vs) = JSArray  [ canonicalise v | v <- vs]
canonicalise   (JSObject vs) = JSObject [ (k, canonicalise v)
                                        | (k,v) <- sortBy (compare `on` fst) vs ]

toAeson :: JSValue -> Aeson.Value
toAeson JSNull        = Aeson.Null
toAeson (JSBool b)    = Aeson.Bool b
toAeson (JSNum n)     = Aeson.Number (fromIntegral n)
toAeson (JSString s)  = Aeson.String (toAesonStr s)
toAeson (JSArray xs)  = Aeson.Array  $ V.fromList  [ toAeson x | x <- xs ]
toAeson (JSObject xs) = Aeson.Object $ KeyMap.fromList [ (toAesonStr k, toAeson v)
                                                       | (k, v) <- xs ]

toAesonStr :: IsString s => JSString -> s
toAesonStr = fromString . fromJSString

-- | As discussed in the haddock docs for 'renderCanonicalJSON', Canonical
-- JSON is /not/ a proper subset of RFC 7159.
--
-- So for the property 'prop_aeson_canonical', where we check that everything
-- produced as canoncal JSON can be parsed by Aeson (which we assume correctly
-- implements RFC 7159), we have to tweak things to keep us within the common
-- subset of canoncal JSON and RFC 7159. Specifically, canoncal JSON only
-- escapes \ and ", and does not escape any other non-printable characters.
--
-- So the tweak is to just omit non-printable characters from all strings.
--
omitNonPrintableChars :: JSValue -> JSValue
omitNonPrintableChars = omitJSValue
  where
    omitJSValue v@JSNull        = v
    omitJSValue v@(JSBool    _) = v
    omitJSValue v@(JSNum     _) = v
    omitJSValue   (JSString  s) = JSString (omitJSString s)
    omitJSValue   (JSArray  vs) = JSArray  [ omitJSValue v | v <- vs]
    omitJSValue   (JSObject vs) = JSObject $ omitDupKeys
                                           [ (omitJSString k, omitJSValue v)
                                           | (k,v) <- vs ]

    omitDupKeys :: [(JSString, JSValue)] -> [(JSString, JSValue)]
    omitDupKeys = nubBy (\a b -> fst a == fst b)

    omitJSString :: JSString -> JSString
    omitJSString = toJSString
                 . filter (\c -> c >= ' ')
                 . fromJSString

instance Arbitrary JSValue where
  arbitrary =
    sized $ \sz ->
    frequency
      [ (1, pure JSNull)
      , (1, JSBool   <$> arbitrary)
      , (2, JSNum    <$> arbitrary)
      , (2, JSString <$> arbitrary)
      , (3, JSArray  <$> resize (sz `div` 2) arbitrary)
      , (3, JSObject . noDupFields
                     <$> resize (sz `div` 2) arbitrary)
      ]
    where
      noDupFields = nubBy (\(x,_) (y,_) -> x==y)

  shrink JSNull        = []
  shrink (JSBool    _) = []
  shrink (JSNum     n) = [ JSNum    n' | n' <- shrink n ]
  shrink (JSString  s) = [ JSString s' | s' <- shrink s ]
  shrink (JSArray  vs) = [ JSArray vs' | vs' <- shrink vs ]
  shrink (JSObject vs) = [ JSObject vs' | vs' <- shrinkList shrinkSnd vs ]
    where
      shrinkSnd (a,b) = [ (a,b') | b' <- shrink b ]

instance Arbitrary Int54 where
  arbitrary = fromIntegral <$>
              frequency [ (1, pure lowerbound)
                        , (1, pure upperbound)
                        , (8, choose (lowerbound, upperbound))
                        ]
    where
      upperbound, lowerbound :: Int64
      upperbound =   999999999999999  -- 15 decimal digits
      lowerbound = (-999999999999999)
  shrink = shrinkIntegral

instance Arbitrary JSString where
  arbitrary = toJSString . getASCIIString <$> arbitrary
  shrink  s = [ toJSString s' | s' <- shrink (fromJSString s)
                              , all (\c -> c >= ' ') s' ]

