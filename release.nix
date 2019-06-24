let
  commonLib = import ./nix/iohk-common.nix;
  disabled = [["nix-tools" "tests" "iohk-monitoring" "tests" "x86_64-darwin"]];
in
{ ... }@args:
commonLib.pkgs.lib.mapAttrsRecursiveCond
(as: !(as ? "type" && as.type == "derivation"))
(path: v: if (builtins.elem path disabled) then null else v)
(commonLib.nix-tools.release-nix {
  package-set-path = ./.;

  # packages from our stack.yaml or plan file (via nix/pkgs.nix) we
  # are interested in building on CI via nix-tools.
  packages = [ "iohk-monitoring" "iohk-monitoring-minimal" ];

  # The set of jobs we consider crutial for each CI run.
  # if a single one of these fails, the build will be marked
  # as failed.
  #
  # The names can be looked up on hydra when in doubt.
  #
  # custom jobs will follow their name as set forth in
  # other-packages.
  #
  # nix-tools packages are prefixed with `nix-tools` and
  # follow the following naming convention:
  #
  #   namespace                      optional cross compilation prefix                 build machine
  #   .-------.                              .-----------------.                .--------------------------.
  #   nix-tools.{libs,exes,tests,benchmarks}.{x86_64-pc-mingw-,}$pkg.$component.{x86_64-linux,x86_64-darwin}
  #             '--------------------------'                    '-------------'
  #                 component type                          cabal pkg and component*
  #
  # * note that for `libs`, $component is empty, as cabal only
  #   provides a single library for packages right now.
  # * note that for `exes`, $component is also empty, because it
  #   it provides all exes under a single result directory.
  #   To  specify a single executable component to build, use
  #   `cexes` as component type.
  #
  # Example:
  #
  #   nix-tools.libs.iohk-monitoring.x86_64-darwin -- will build the iohk-monitoring library on and for macOS
  #   nix-tools.libs.x86_64-pc-mingw32-iohk-monitoring.x86_64-linux -- will build the iohk-monitoring library on linux for windows.
  #   nix-tools.tests.iohk-monitoring.tests.x86_64-linux -- will build and run the tests from the
  #                                                          iohk-monitoring package on linux.

  # The required jobs that must pass for ci not to fail:
  required-name = "iohk-monitoring-required-checks";
  required-targets = jobs: [
    # targets are specified using above nomenclature:
    jobs.nix-tools.tests.iohk-monitoring.tests.x86_64-linux

    jobs.nix-tools.exes.iohk-monitoring.x86_64-linux

    # Linux "minimal" cabal flags builds
    jobs.nix-tools.libs.iohk-monitoring-minimal.x86_64-linux
    jobs.nix-tools.exes.iohk-monitoring-minimal.x86_64-linux

    # Disabled due to: https://github.com/psibi/download/issues/17:
    #jobs.nix-tools.exes.x86_64-pc-mingw32-iohk-monitoring.x86_64-linux

  ];
} args)
