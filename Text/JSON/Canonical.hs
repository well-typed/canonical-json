--------------------------------------------------------------------
-- |
-- Module    : Text.JSON.Canonical
-- Copyright : (c) Duncan Coutts 2017
--
--
-- An implementation of Canonical JSON.
--
-- <http://wiki.laptop.org/go/Canonical_JSON>
--
-- The \"canonical JSON\" format is designed to provide repeatable hashes of
-- JSON-encoded data. It is designed for applications that need to hash, sign
-- or authenitcate JSON data structures.
--
-- The format is an extended subset of the normal JSON format.
--
-- Canonical JSON is parsable with any full JSON parser, and it allows
-- whitespace for pretty-printed human readable presentation, but it can be put
-- into a canonical form which then has a stable serialised representation and
-- thus a stable hash.
--
-- The basic concept is that a file in the canonical JSON format can be read
-- using 'parseCanonicalJSON'. Note that this input file does /not/ itself need
-- to be in canonical form, it just needs to be in the canonical JSON format.
-- Then the 'renderCanonicalJSON' function is used to render into the canonical
-- form. This is then the form that can be hashed or signed etc.
--
-- The 'prettyCanonicalJSON' is for convenience to render in a human readable
-- style, since the canoncal form eliminates unnecessary white space which
-- makes the output hard to read. This style is again suitable to read using
-- 'parseCanonicalJSON'. So this is suitable to use for producing output that
-- has to be later hashed or otherwise checked.
--
-- Known bugs\/limitations:
--
--  * Decoding\/encoding Unicode code-points beyond @U+00ff@ is currently broken
--
module Text.JSON.Canonical (
    -- * Types
    JSValue(..)
  , Int54
  , JSString
    -- * Parsing and printing
  , parseCanonicalJSON
  , renderCanonicalJSON
  , prettyCanonicalJSON
    -- * Type classes
  , ToJSON(..)
  , FromJSON(..)
  , ToObjectKey(..)
  , FromObjectKey(..)
  , ReportSchemaErrors(..)
  , Expected
  , Got
  , expectedButGotValue
    -- * Utility
  , toJSString
  , fromJSString
  , fromJSObject
  , fromJSField
  , fromJSOptField
  , mkObject
  ) where

import Text.JSON.Canonical.Types
import Text.JSON.Canonical.Parse
import Text.JSON.Canonical.Class

