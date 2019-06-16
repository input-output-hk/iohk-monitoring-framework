# This is the nix-shell environment used by "stack --nix".
# With Nix integration enabled, Stack will run all GHC commands
# within a nix-shell defined by this file.
#
# The shell contains the correct version of GHC and all system
# libraries needed to build this project and its dependencies.

with import ../lib.nix;
with pkgs;

let
  stack-pkgs = import ./.stack.nix;
  compiler = (stack-pkgs.extras {}).compiler.nix-name;

in haskell.lib.buildStackProject {
  name = "iohk-monitoring-env";
  buildInputs = [ zlib openssl git systemd ];
  ghc = haskell.packages.${compiler}.ghc;
}
