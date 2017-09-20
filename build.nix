let
    localPackages = [
        "kontiki-raft-classes"
        "kontiki-raft"
        "kontiki"
        "monad-logger-katip"
        "transformers-metrics"
    ];

    dontCheckPackages = [
        "proto3-suite"
        "grpc-haskell"
    ];

    doJailbreakPackages = [
        "indexed-extras"
    ];

    bootstrap = import <nixpkgs> { };
    nixpkgs = builtins.fromJSON (builtins.readFile ./nix/nixpkgs.json);

    src = bootstrap.fetchFromGitHub {
        owner = "NixOS";
        repo = "nixpkgs";
        inherit (nixpkgs) rev sha256;
    };

    config = {
        packageOverrides = pkgs: rec {
            grpc = pkgs.callPackage ./nix/grpc.nix { };

            haskellPackages =
                let
                    generatedLocalPackages = haskellPackagesNew: haskellPackagesOld:
                        let
                            toPackage = name: {
                                inherit name;
                                value = haskellPackagesNew.callPackage (./. + "/${name}/default.nix") { };
                            };
                        in builtins.listToAttrs (map toPackage localPackages);

                    generatedOverrides = haskellPackagesNew: haskellPackagesOld:
                        let
                            filterNixFiles = entries:
                                let
                                    pred = n: v:
                                        n != "grpc.nix" && pkgs.stdenv.lib.hasSuffix ".nix" n && v == "regular";
                                in
                                pkgs.stdenv.lib.filterAttrs pred entries;

                            toPackage = file: _: {
                                name = builtins.replaceStrings [ ".nix" ][ "" ] file;
                                value = haskellPackagesNew.callPackage (./. + "/nix/${file}") { };
                            };
                        in
                            pkgs.lib.mapAttrs' toPackage (filterNixFiles (builtins.readDir ./nix));

                    makeOverrides = function: names: haskellPackagesNew: haskellPackagesOld:
                        let
                            toPackage = name: {
                                inherit name;
                                value = function haskellPackagesOld.${name};
                            };
                        in
                            builtins.listToAttrs (map toPackage names);

                    composeExtensionsList =
                        pkgs.lib.fold pkgs.lib.composeExtensions (_: _: {});
                in
                    pkgs.haskellPackages.override {
                        overrides = composeExtensionsList [
                            generatedLocalPackages
                            generatedOverrides
                            (makeOverrides pkgs.haskell.lib.dontCheck dontCheckPackages)
                            (makeOverrides pkgs.haskell.lib.doJailbreak doJailbreakPackages)
                        ];
                    };
        };
    };

    pkgs = import src { inherit config; };
in
    builtins.listToAttrs (map (name: { inherit name; value = pkgs.haskellPackages.${name}; }) localPackages)
