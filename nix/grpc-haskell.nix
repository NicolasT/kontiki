{ mkDerivation, async, base, bytestring, c2hs, clock, containers
, criterion, fetchgit, grpc, managed, pipes, proto3-suite
, proto3-wire, QuickCheck, random, safe, sorted-list, stdenv, stm
, tasty, tasty-hunit, tasty-quickcheck, text, time, transformers
, turtle, unix, vector
}:
mkDerivation {
  pname = "grpc-haskell";
  version = "0.0.0.0";
  src = fetchgit {
    url = "https://github.com/awakesecurity/gRPC-haskell.git";
    sha256 = "1ninqwjknbagjfjhqhif3mal1plmnnpg631b8dgyx5m880yrdl1p";
    rev = "679e3d7ded733f0171723236e14f28832bfdff65";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base bytestring clock containers managed pipes proto3-suite
    proto3-wire safe sorted-list stm tasty tasty-hunit tasty-quickcheck
    transformers vector
  ];
  librarySystemDepends = [ grpc ];
  libraryToolDepends = [ c2hs ];
  testHaskellDepends = [
    async base bytestring clock containers managed pipes proto3-suite
    QuickCheck safe tasty tasty-hunit tasty-quickcheck text time
    transformers turtle unix
  ];
  benchmarkHaskellDepends = [
    async base bytestring criterion proto3-suite random
  ];
  homepage = "https://github.com/awakenetworks/gRPC-haskell";
  description = "Haskell implementation of gRPC layered on shared C library";
  license = stdenv.lib.licenses.asl20;
}
