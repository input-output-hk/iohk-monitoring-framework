{ pkgs }:
let
  # overriding logic so we can pass -I to nix, and overide the
  # relevant import.
  overrideWith = override: default:
   let
     try = builtins.tryEval (builtins.findFile builtins.nixPath override);
   in if try.success then
     builtins.trace "using search host <${override}>" try.value
   else
     default;
in rec {
  # a different haskell infrastructure
  haskell = import (overrideWith "haskell"
                    (pkgs.fetchFromGitHub { owner  = "input-output-hk";
                                            repo   = "haskell.nix";
                                            rev    = "3584345a9ab001d1867e972a1a20b4406cbffd68";
                                            sha256 = "08pzfvspfl5nvn5szy7bv3rbwymjgmbdm1ac571c64fzhrwf5ghw";
                                            name   = "haskell-lib-source"; }))
                   { inherit pkgs; };
}