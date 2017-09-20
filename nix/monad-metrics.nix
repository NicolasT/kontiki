{ mkDerivation, base, clock, ekg-core, hashable, microlens, mtl
, stdenv, text, transformers, unordered-containers
}:
mkDerivation {
  pname = "monad-metrics";
  version = "0.2.0.0";
  sha256 = "04rnblrxvax8pns5xp2ic04c9s78cikyphsw5w2894ll08b3srh9";
  libraryHaskellDepends = [
    base clock ekg-core hashable microlens mtl text transformers
    unordered-containers
  ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/sellerlabs/monad-metrics#readme";
  description = "A convenient wrapper around EKG metrics";
  license = stdenv.lib.licenses.mit;
}
