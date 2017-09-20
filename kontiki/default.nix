{ mkDerivation, aeson, async, base, bytestring, clock, containers
, criterion, data-default, deepseq, dhall, ekg, ekg-core
, exceptions, grpc-haskell, hashable, hedgehog, hedgehog-quickcheck
, hspec, hspec-expectations, katip, kontiki-raft
, kontiki-raft-classes, lens, leveldb-haskell, lifted-async
, lifted-base, monad-control, monad-logger, monad-logger-katip
, monad-metrics, mtl, network, optparse-applicative, proto3-suite
, proto3-wire, QuickCheck, quickcheck-text, random, safe-exceptions
, stdenv, stm, suspend, tasty, tasty-hedgehog, tasty-hspec
, temporary, text, timers, transformers, transformers-base
, transformers-metrics, unordered-containers, vector
}:
mkDerivation {
  pname = "kontiki";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring clock containers data-default dhall ekg
    ekg-core exceptions grpc-haskell hashable katip kontiki-raft
    kontiki-raft-classes lens leveldb-haskell lifted-async lifted-base
    monad-control monad-logger monad-logger-katip monad-metrics mtl
    network optparse-applicative proto3-suite proto3-wire QuickCheck
    quickcheck-text random safe-exceptions stm suspend text timers
    transformers transformers-base transformers-metrics
    unordered-containers vector
  ];
  executableHaskellDepends = [
    base data-default grpc-haskell kontiki-raft-classes lens
  ];
  testHaskellDepends = [
    base bytestring hedgehog hedgehog-quickcheck hspec
    hspec-expectations kontiki-raft kontiki-raft-classes
    leveldb-haskell monad-logger monad-metrics proto3-suite QuickCheck
    tasty tasty-hedgehog tasty-hspec temporary text
  ];
  benchmarkHaskellDepends = [
    async base criterion deepseq grpc-haskell katip monad-metrics
    safe-exceptions
  ];
  homepage = "https://github.com/NicolasT/kontiki/tree/master/kontiki#readme";
  description = "An implementation of the Raft consensus protocol - Server";
  license = stdenv.lib.licenses.asl20;
}
