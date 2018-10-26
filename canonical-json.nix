{ mkDerivation, base, bytestring, containers, parsec, pretty
, stdenv
}:
mkDerivation {
  pname = "canonical-json";
  version = "0.5.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers parsec pretty
  ];
  description = "Canonical JSON for signing and hashing JSON values";
  license = stdenv.lib.licenses.bsd3;
}
