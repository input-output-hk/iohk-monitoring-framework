############################################################################
#
# Hydra release jobset.
#
# The purpose of this file is to select jobs defined in default.nix and map
# them to all supported build platforms.
#
############################################################################

# The project sources
{ iohk-monitoring ? { outPath = ./.; rev = "abcdef"; }

# Function arguments to pass to the project
, projectArgs ? {
    config = { allowUnfree = false; inHydra = true; };
    gitrev = iohk-monitoring.rev;
  }

# The systems that the jobset will be built for.
, supportedSystems ? [ "x86_64-linux" ]

# The systems used for cross-compiling
, supportedCrossSystems ? [ "x86_64-linux" ]

# A Hydra option
, scrubJobs ? true

# Dependencies overrides
, sourcesOverride ? {}

# Import pkgs, including IOHK common nix lib
, pkgs ? import ./nix { inherit sourcesOverride; }

}:

with (import pkgs.iohkNix.release-lib) {
  inherit pkgs;
  inherit supportedSystems supportedCrossSystems scrubJobs projectArgs;
  packageSet = import iohk-monitoring;
  gitrev = iohk-monitoring.rev;
};

with pkgs.lib;

let
  requiredSupportedSystems = [ "x86_64-linux"  "x86_64-darwin" ];
  # Recurse through an attrset, returning all derivations in a list.
  collectComponents' = ds: filter (d: elem d.system requiredSupportedSystems) (collect isDerivation ds);
  # Adds the package name to the derivations for windows-testing-bundle.nix
  # (passthru.identifier.name does not survive mapTestOn)
  collectComponents = ds: concatLists (
    mapAttrsToList (packageName: package:
      map (drv: drv // { inherit packageName; }) (collectComponents' package)
    ) ds);

  # inherit (systems.examples) mingwW64 musl64;

  jobs = {
    native = mapTestOn (__trace (__toJSON (packagePlatforms project)) (packagePlatforms project));
    # "${mingwW64.config}" = mapTestOnCross mingwW64 (packagePlatformsCross project);
  } // (mkRequiredJob (
      collectComponents jobs.native.tests ++
      collectComponents jobs.native.benchmarks ++
      collectComponents jobs.native.libs ++
      collectComponents jobs.native.exes
      ));

in jobs
