{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        monoSkiaPkg = pkgs.callPackage ./skiasharp-package.nix {};
        hPkgs = pkgs.haskell.packages."ghc966";

        devDeps = [
          pkgs.pkg-config

          hPkgs.cabal-install
          hPkgs.ghc
          hPkgs.fourmolu
          hPkgs.haskell-language-server
        ];

        libDeps = with pkgs; [
          # zlib is used by a lot of libraries apparently.
          # (Specifically package 'warp' needs `-lz`, 'warp' is needed by `stack hoogle --server`)
          zlib

          # skia
          monoSkiaPkg
          gn
          gcc
          fontconfig
          expat # required by 'fontconfig'
        ];

        pyDeps = [
            pkgs.python312
        ] ++ (with pkgs.python311Packages; [
            pycparser
            pytest
        ]);
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = devDeps ++ libDeps ++ pyDeps;
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath libDeps;
        };
      });
}

