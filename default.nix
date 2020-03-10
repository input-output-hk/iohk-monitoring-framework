{ target ? builtins.currentSystem
# allows to cutomize haskellNix (ghc and profiling, see ./nix/haskell.nix)
, config ? {}
, localLib ? import ./lib.nix
# allows to override dependencies of the project without modifications,
# eg. to test build against local checkout of iohk-nix:
# nix build -f default.nix cardano-node --arg sourcesOverride '{
#   iohk-nix = ../iohk-nix;
# }'
, sourcesOverride ? {}
}:
let
  systemTable = {
    x86_64-windows = builtins.currentSystem;
  };
  crossSystemTable = {
    x86_64-windows = lib.systems.examples.mingwW64;
  };
  system = systemTable.${target} or target;
  crossSystem = crossSystemTable.${target} or null;
  pkgs = localLib.iohkNix.getPkgsDefault { inherit system config; };
  # pkgs ? import ./nix { inherit system crossSystem config sourcesOverride; }
  inherit (import ./nix { inherit system crossSystem config sourcesOverride; }) commonLib iohkMonitoringHaskellPackages;
  gitrev = pkgs.iohkNix.commitIdFromGitRepoOrZero ./.git;
  lib = pkgs.lib;
in
with pkgs; with commonLib;
let
  haskellPackages = recRecurseIntoAttrs
    # we are only intersted in listing the project packages:
    (selectProjectPackages iohkMonitoringHaskellPackages);

  self = {
    inherit haskellPackages;
    inherit (haskellPackages.iohk-monitoring.identifier) version;

    # `tests` are the test suites which have been built.
    tests = collectComponents' "tests" haskellPackages;
    # `benchmarks` (only built, not run).
    benchmarks = collectComponents' "benchmarks" haskellPackages;

    libs = collectComponents' "library"
      (removeAttrs haskellPackages ["lobemo-examples"]);

    exes = collectComponents' "exes" haskellPackages;

    checks = recurseIntoAttrs {
      # `checks.tests` collect results of executing the tests:
      tests = collectChecks haskellPackages;
    };

    shell = import ./shell.nix {
      inherit pkgs;
      withHoogle = true;
    };
};
in self
