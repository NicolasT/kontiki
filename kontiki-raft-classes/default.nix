{ mkDerivation, base, containers, contravariant, indexed
, indexed-extras, QuickCheck, stdenv, tasty, tasty-quickcheck
, transformers
}:
mkDerivation {
  pname = "kontiki-raft-classes";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers contravariant indexed indexed-extras QuickCheck
    tasty tasty-quickcheck transformers
  ];
  homepage = "https://github.com/NicolasT/kontiki/tree/master/kontiki-raft-classes#readme";
  description = "An implementation of the Raft consensus protocol - Classes";
  license = stdenv.lib.licenses.asl20;
}
