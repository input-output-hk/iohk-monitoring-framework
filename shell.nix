with (import <nixpkgs> {});
stdenv.mkDerivation {
  name = "srcEnv";
  nativeBuildInputs = [ cabal-install
                        haskellPackages.ghcid
                        zlib libiconv
                        ghc
                      ];
  buildInputs = [
                ];
}

