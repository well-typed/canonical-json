{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

import           Data.Int
import           Data.List (sortBy, nubBy)
import           Data.Function (on)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Text.JSON.Canonical

import qualified Data.Aeson as Aeson (Value (..), eitherDecode)
import           Data.String (fromString)
import qualified Data.Vector         as V  (fromList)
import qualified Data.HashMap.Strict as HM (fromList)

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
    Aeson.eitherDecode (renderCanonicalJSON jsval) == Right (toAeson jsval)

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
toAeson (JSString s)  = Aeson.String (fromString s)
toAeson (JSArray xs)  = Aeson.Array  $ V.fromList  [ toAeson x | x <- xs ]
toAeson (JSObject xs) = Aeson.Object $ HM.fromList [ (fromString k, toAeson v)
                                                   | (k, v) <- xs ]

instance Arbitrary JSValue where
  arbitrary =
    sized $ \sz ->
    frequency
      [ (1, pure JSNull)
      , (1, JSBool   <$> arbitrary)
      , (2, JSNum    <$> arbitrary)
      , (2, JSString . getASCIIString <$> arbitrary)
      , (3, JSArray                <$> resize (sz `div` 2) arbitrary)
      , (3, JSObject . mapFirst getASCIIString .  noDupFields <$> resize (sz `div` 2) arbitrary)
      ]
    where
      noDupFields = nubBy (\(x,_) (y,_) -> x==y)
      mapFirst f = map (\(x, y) -> (f x, y))

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

