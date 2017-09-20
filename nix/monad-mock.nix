{ mkDerivation, base, constraints, exceptions, haskell-src-exts
, haskell-src-meta, hspec, monad-control, mtl, stdenv
, template-haskell, th-orphans, transformers-base
}:
mkDerivation {
  pname = "monad-mock";
  version = "0.1.1.2";
  sha256 = "029c8jcw7y3hd1llvfnm85fwxvfh7mlhr7dxnfsx6x8zq1qda12f";
  libraryHaskellDepends = [
    base constraints exceptions haskell-src-exts haskell-src-meta
    monad-control mtl template-haskell th-orphans transformers-base
  ];
  testHaskellDepends = [ base hspec ];
  homepage = "https://github.com/cjdev/monad-mock#readme";
  description = "A monad transformer for mocking mtl-style typeclasses";
  license = stdenv.lib.licenses.isc;
}
