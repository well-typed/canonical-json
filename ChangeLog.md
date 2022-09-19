# Revision history for canonical-json

## 0.6.0.1 2022-09-19

* Support GHC 9.2 and 9.4, and bytestring 0.11.x

* Support recent versions of QuickCheck and Aeson in the tests

* Fix the domain of a property test `prop_aeson_canonical`

* Add CI on github covering GHC versions 8.0, 8.2, 8.4, 8.6, 8.8, 8.10, 9.2

## 0.6.0.0 2019-07-31

* Introduced JSString type rather than using String, for improved memory use.

* Improved parser performance

* Reduce stack space usage and introduce stack use tests

* Added benchmarks

## 0.5.0.1 2018-10-26

* ghc-8.4 compatibility.
* Move the test suite from the hackage-security library

## 0.5.0.0 2017-09-06

* Canonical JSON code extracted from hackage-security-0.5.2.2 into
  separate library.
