
import Text.JSON.Canonical.Parse

import qualified Data.ByteString.Lazy as BS

import Criterion.Main

main :: IO ()
main = defaultMain [
  env loadData $ \jsondata ->
  bench "parse" $ nf parseCanonicalJSON jsondata
  ]

loadData :: IO BS.ByteString
loadData = BS.readFile "benchmark/cardano-genesis.json"
