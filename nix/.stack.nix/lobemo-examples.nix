{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "2.0";
      identifier = { name = "lobemo-examples"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "Alexander Diemand";
      homepage = "https://github.com/input-output-hk/iohk-monitoring-framework";
      url = "";
      synopsis = "examples of logging, benchmarking, and monitoring";
      description = "";
      buildType = "Simple";
      };
    components = {
      exes = {
        "example-simple" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.base)
            (hsPkgs.aeson)
            (hsPkgs.iohk-monitoring)
            (hsPkgs.lobemo-backend-editor)
            (hsPkgs.lobemo-scribe-systemd)
            (hsPkgs.async)
            (hsPkgs.bytestring)
            (hsPkgs.mtl)
            ] ++ (if system.isWindows
            then [ (hsPkgs.Win32) ]
            else [ (hsPkgs.unix) ]);
          };
        "example-complex" = {
          depends = ([
            (hsPkgs.aeson)
            (hsPkgs.base)
            (hsPkgs.iohk-monitoring)
            (hsPkgs.lobemo-backend-aggregation)
            (hsPkgs.lobemo-backend-editor)
            (hsPkgs.lobemo-backend-ekg)
            (hsPkgs.lobemo-backend-monitoring)
            (hsPkgs.lobemo-scribe-systemd)
            (hsPkgs.async)
            (hsPkgs.bytestring)
            (hsPkgs.mtl)
            (hsPkgs.random)
            (hsPkgs.text)
            (hsPkgs.tracer-transformers)
            (hsPkgs.unordered-containers)
            ] ++ (if system.isWindows
            then [ (hsPkgs.Win32) ]
            else [
              (hsPkgs.unix)
              ])) ++ (pkgs.lib).optional (system.isLinux) (hsPkgs.download);
          };
        "example-performance" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.iohk-monitoring)
            (hsPkgs.async)
            (hsPkgs.criterion)
            (hsPkgs.text)
            (hsPkgs.unordered-containers)
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././examples; }