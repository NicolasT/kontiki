{ mkDerivation, aeson, base, containers, data-default
, data-default-class, hspec-expectations, indexed, indexed-extras
, kontiki-raft-classes, lens, monad-logger, monad-mock, mtl
, QuickCheck, rebindable, stdenv, tasty, tasty-hspec, text
, transformers
}:
mkDerivation {
  pname = "kontiki-raft";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base containers data-default-class indexed indexed-extras
    kontiki-raft-classes lens monad-logger mtl rebindable text
    transformers
  ];
  testHaskellDepends = [
    base containers data-default hspec-expectations indexed-extras
    kontiki-raft-classes lens monad-logger monad-mock mtl QuickCheck
    tasty tasty-hspec transformers
  ];
  homepage = "https://github.com/NicolasT/kontiki/tree/master/kontiki-raft#readme";
  description = "An implementation of the Raft consensus protocol - Protocol";
  license = stdenv.lib.licenses.asl20;
}
