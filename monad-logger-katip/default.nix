{ mkDerivation, aeson, base, conduit-extra, exceptions, fast-logger
, katip, lifted-async, mmorph, monad-control, monad-logger, mtl
, primitive, resourcet, should-not-typecheck, stdenv, stm, tasty
, tasty-hspec, text, transformers, transformers-base
}:
mkDerivation {
  pname = "monad-logger-katip";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base conduit-extra exceptions fast-logger katip mmorph
    monad-control monad-logger mtl primitive resourcet text
    transformers transformers-base
  ];
  testHaskellDepends = [
    aeson base katip lifted-async monad-logger mtl should-not-typecheck
    stm tasty tasty-hspec text
  ];
  homepage = "https://github.com/NicolasT/kontiki/tree/master/monad-logger-katip#readme";
  description = "A Katip handler for MonadLogger";
  license = stdenv.lib.licenses.asl20;
}
