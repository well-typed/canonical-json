{ mkDerivation, base, bytestring, containers, parsec, pretty
, stdenv
}:
mkDerivation {
  pname = "canonical-json";
  version = "0.6.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers parsec pretty
  ];
  description = "Canonical JSON for signing and hashing JSON values";
  license = stdenv.lib.licenses.bsd3;
}
