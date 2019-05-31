with (import ../../default.nix {});

nix-tools._raw.shellFor {
  packages = ps: with ps; [ iohk-monitoring contra-tracer ];
  buildInputs = [ nix-tools.exes.liquidhaskell ];
}
