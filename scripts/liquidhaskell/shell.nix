with (import ../../default.nix {});

nix-tools._raw.shellFor {
  packages = ps: with ps; [ iohk-monitoring ];
  buildInputs = [ nix-tools.exes.liquidhaskell ];
}
