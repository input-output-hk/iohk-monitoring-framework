{ pkgs ? import <nixpkgs> {}
, iohk-extras ? {}
, iohk-module ? {}
, haskell
, ...
}:
let

  # our packages
  stack-pkgs = import ./.stack.nix;

  # Build the packageset with module support.
  # We can essentially override anything in the modules
  # section.
  #
  #  packages.cbors.patches = [ ./one.patch ];
  #  packages.cbors.flags.optimize-gmp = false;
  #
  compiler = (stack-pkgs.extras haskell.hackage).compiler.nix-name;
  pkgSet = haskell.mkStackPkgSet {
    inherit stack-pkgs;
    pkg-def-extras = [
      iohk-extras.${compiler}
    ];
    modules = [
      iohk-module

      {
        # Packages we wish to ignore version bounds of.
        # This is similar to jailbreakCabal, however it
        # does not require any messing with cabal files.
        packages.katip.components.library.doExactConfig = true;
        # lift containers==0.5.* restriction, see
        # https://github.com/bitnomial/prometheus/commit/61bb7ec834279d6274c9d13b0edcfff3cc42c856
        packages.prometheus.components.library.doExactConfig = true;
      }
    ];
  };

in
  pkgSet.config.hsPkgs // { _config = pkgSet.config; }
