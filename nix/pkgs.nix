{ pkgs ? import <nixpkgs> {}
, iohk-overlay ? {}
, iohk-module ? {}
, haskell
, hackage
, stackage
, ...
}:
let

  # our packages
  stack-pkgs = import ./.stack-pkgs.nix;

  # packages which will require TH and thus
  # will need -fexternal-interpreter treatment
  # when cross compiling.
  th-packages = [
          "aeson"
          "bifunctors"
          "exceptions"
          "free"
          "invariant"
          "iohk-monitoring"
          "katip"
          "lens"
          "microlens-th"
          "reflection"
          "semigroupoids"
          "tagged"
          "th-abstraction"
          "yaml"
        ];

  # Build the packageset with module support.
  # We can essentially override anything in the modules
  # section.
  #
  #  packages.cbors.patches = [ ./one.patch ];
  #  packages.cbors.flags.optimize-gmp = false;
  #
  compiler = (stack-pkgs.overlay hackage).compiler.nix-name;
  pkgSet = haskell.mkNewPkgSet {
    inherit pkgs;
    pkg-def = stackage.${stack-pkgs.resolver};
    pkg-def-overlays = [
      stack-pkgs.overlay
      iohk-overlay.${compiler}
    ];
    modules = [
      haskell.ghcHackagePatches.${compiler}

      (iohk-module { nixpkgs = pkgs;
                     inherit th-packages; })


      {
        # Packages we wish to ignore version bounds of.
        # This is similar to jailbreakCabal, however it
        # does not require any messing with cabal files.
        packages.katip.components.library.doExactConfig = true;

        packages.iohk-monitoring.components.tests.tests.setupTestFlags = [
          "--show-details=streaming"
        ];
      }
    ];
  };

in
  pkgSet.config.hsPkgs // { _config = pkgSet.config; }  
