let
  addRealTimeTestLogs = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    testTarget = "--show-details=streaming";
  });

  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskell.packages.ghc863.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          iohk-monitoring = pkgs.haskell.lib.dontHaddock (haskellPackagesNew.callPackage ./iohk-monitoring.nix { });
        };
      };
    };
  };

  nixpkgs = builtins.fetchTarball {
    url    = "https://github.com/NixOS/nixpkgs/archive/6054276a8e6e877f8846c79b0779e3310c495a6b.tar.gz";
    sha256 = "11mv8an4zikh2hybn11znqcbxazqq32byvvvaymy2xkpww2jnkxp";
  };

  pkgs = import nixpkgs { inherit config; };
in
  { iohk-monitoring = addRealTimeTestLogs pkgs.haskellPackages.iohk-monitoring;
  }
