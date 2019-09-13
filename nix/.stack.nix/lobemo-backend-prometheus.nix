{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "2.0";
      identifier = { name = "lobemo-backend-prometheus"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "Alexander Diemand";
      homepage = "https://github.com/input-output-hk/iohk-monitoring-framework";
      url = "";
      synopsis = "provides a backend implementation to Prometheus";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.iohk-monitoring)
          (hsPkgs.lobemo-backend-ekg)
          (hsPkgs.prometheus)
          (hsPkgs.ekg-prometheus-adapter)
          (hsPkgs.async)
          (hsPkgs.ekg)
          (hsPkgs.warp)
          ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././plugins/backend-prometheus; }