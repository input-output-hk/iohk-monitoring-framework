let
   config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskell.packages.ghc843.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          iohk-monitoring = pkgs.haskell.lib.dontHaddock (haskellPackagesNew.callPackage ./iohk-monitoring.nix { });
        };
      };
    };
  };
  nixpkgs = builtins.fetchTarball {
    url    = "https://github.com/NixOS/nixpkgs/archive/069bf7aee30faf7b3ed773cfae2154d761b2d6c2.tar.gz";
    sha256 = "1c44vjb60fw2r8ck8yqwkj1w4288wixi59c6w1vazjixa79mvjvg";
  };

  pkgs = import nixpkgs { inherit config; };
in
  { iohk-monitoring = pkgs.haskellPackages.iohk-monitoring;
  }
