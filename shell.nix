{ withHoogle ? true
, localLib ? import ./lib.nix
}:
let
  pkgs = localLib.iohkNix.pkgs;
  default = import ./default.nix {};
  devops = pkgs.stdenv.mkDerivation {
    name = "devops-shell";
    buildInputs = [
      localLib.niv
    ];
    shellHook = ''
      echo "DevOps Tools" \
      | ${pkgs.figlet}/bin/figlet -f banner -c \
      | ${pkgs.lolcat}/bin/lolcat

      echo "NOTE: you may need to export GITHUB_TOKEN if you hit rate limits with niv"
      echo "Commands:
        * niv update <package> - update package

      "
    '';
  };
in
default.nix-tools._raw.shellFor {
  packages    = ps: with ps; [
    iohk-monitoring
    lobemo-backend-aggregation
    lobemo-backend-editor
    lobemo-backend-ekg
    lobemo-backend-graylog
    lobemo-backend-monitoring
    lobemo-examples
    lobemo-scribe-systemd
  ];
  withHoogle  = withHoogle;
  buildInputs =
  (with default.nix-tools._raw; [
    cabal-install.components.exes.cabal
    ghcid.components.exes.ghcid
  ]) ++
  (with default.nix-tools._raw._config._module.args.pkgs; [
    tmux
    zlib
    libiconv
    cabal2nix
    stack
    numactl
  ]);
} // { inherit devops; }
