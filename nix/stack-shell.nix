
with import ./. {};

haskell.lib.buildStackProject {
  name = "stack-env";
  buildInputs = with pkgs; [ zlib openssl gmp libffi git pkg-config systemd haskellPackages.happy stylish-haskell ];
  ghc = (import ../shell.nix {inherit pkgs;}).ghc.baseGhc;
}
