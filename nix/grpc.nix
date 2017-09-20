{ stdenv, fetchurl, c-ares, openssl, pkgconfig, protobuf, which, zlib
}:

stdenv.mkDerivation rec {
    name = "grpc-${version}";
    version = "1.3.9";
    rev = "28aee694bd7d50104b57da59cec5bc1b8835675b";
    src = fetchurl {
        url = "https://github.com/grpc/grpc/archive/v${version}.tar.gz";
        sha256 = "bdd67b43e4bc88801b236479381fe7ebcad09fd339d78c45feaab0200e53acae";
    };
    /* Need 'which' because that's how the Makefile looks for 'protoc' */
    nativeBuildInputs = [ pkgconfig protobuf which ];
    buildInputs = [ c-ares openssl zlib ];
    /* GRPC's Makefile is pretty broken:
     * - "AR" is taken from the envonment, but default also includes required
     *   options, even though there is an "AROPTS" variable used as well
     * - Arguments put in "LDFLAGS" are actually GCC argument (using "-Wl,..."),
     *   but "LD" is taken from the environment and "LDFLAGS" passed to it
     *   as-is.
     */
    makeFlags = [ "AROPTS=rcs" "LD=gcc" ];
    preInstall = "export prefix";
    enableParallelBuilding = true;
}
