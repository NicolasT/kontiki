{ mkDerivation, base, exceptions, katip, mmorph, monad-control
, monad-logger, monad-metrics, mtl, primitive, resourcet, stdenv
, transformers, transformers-base
}:
mkDerivation {
  pname = "transformers-metrics";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base exceptions katip mmorph monad-control monad-logger
    monad-metrics mtl primitive resourcet transformers
    transformers-base
  ];
  homepage = "https://github.com/NicolasT/kontiki/tree/master/transformers-metrics#readme";
  description = "A monad transformer for monad-metrics' MonadMetrics";
  license = stdenv.lib.licenses.asl20;
}
