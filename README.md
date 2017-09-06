An implementation of [Canonical JSON].

The "canonical JSON" format is designed to provide repeatable hashes of
JSON values. It is designed for applications that need to hash, sign
or authenitcate JSON data structures., including embedded signatures.

Canonical JSON is parsable with any full JSON parser, and it allows white
space for pretty-printed human readable presentation, but it can be put into
a canonical form which then has a stable serialised representation and thus a
stable hash.

The basic concept is that a file in the canonical JSON format can be read
using `parseCanonicalJSON`. Note that this input file does *not* itself need
to be in canonical form, it just needs to be in the canonical JSON format.
Then the `renderCanonicalJSON` function is used to render into the canonical
form. This is then the form that can be hashed or signed etc.

The `prettyCanonicalJSON` is for convenience to render in a human readable
style, since the canoncal form eliminates unnecessary white space which
makes the output hard to read. This style is again suitable to read using
'parseCanonicalJSON'. So this is suitable to use for producing output that
has to be later hashed or otherwise checked.

See the [API docs] on Hackage.

This package has been extracted from the [hackage-security] package where
canonical JSON is used for all the signed TUF files, such as the
[root keys file], etc. As you can see from that, canoncal JSON allows keeping
JSON files in a human readable pretty-printed form, and still allows verifying
signatures. In particular this demonstrates the use of embedded signatures,
where the `root.json` both contains a body value and multiple signatures of
that body all within the same file. This is because canoncal JSON is about
hashes for *JSON values, not serialised JSON text*.

[Canonical JSON]: http://wiki.laptop.org/go/Canonical_JSON
[API docs]: https://hackage.haskell.org/package/canonical-json
[hackage-security]: https://hackage.haskell.org/package/hackage-security
[root keys file]: https://hackage.haskell.org/root.json


Known bugs limitations
-----------------------

 * Decoding/encoding Unicode code-points beyond `U+00ff` is currently broken

