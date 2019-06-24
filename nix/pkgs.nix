{ pkgs ? import <nixpkgs> {}
# package set extensions from iohk-nix
, iohk-extras ? {}
# package set configuration from iohk-nix
, iohk-module ? {}
# Haskell.nix library
, haskell
# iohk-nix might call this with more arguments
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
      # Add a clone of iohk-monitoring package
      (hackage: { packages.iohk-monitoring-minimal = ./.stack.nix/iohk-monitoring.nix; })
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

        # Add a variant of iohk-monitoring to test disabling of flags
        packages.iohk-monitoring-minimal.flags = {
          disable-aggregation = true;
          disable-ekg = true;
          disable-graylog = true;
          disable-prometheus = true;
          disable-gui = true;
          disable-monitoring = true;
          disable-observables = true;
          disable-syslog = true;
          # Keep examples, to see if they build
          disable-examples = false;
        };
      }
    ];
  };

in
  pkgSet.config.hsPkgs // { _config = pkgSet.config; }
