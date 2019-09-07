{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "0.2";
      identifier = { name = "ekg-prometheus-adapter"; version = "0.2.0.1"; };
      license = "MIT";
      copyright = "2016 Alfredo Di Napoli";
      maintainer = "operations@iohk.io";
      author = "Alfredo Di Napoli";
      homepage = "https://github.com/adinapoli/ekg-prometheus-adapter";
      url = "https://github.com/CodiePP/ekg-prometheus-adapter";
      synopsis = "tracer transformers and examples showing their use";
      description = "Easily expose your EKG metrics to Prometheus";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.prometheus)
          (hsPkgs.ekg-core)
          (hsPkgs.unordered-containers)
          (hsPkgs.containers)
          (hsPkgs.text)
          (hsPkgs.transformers)
          (hsPkgs.microlens-th)
          ];
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.ekg-prometheus-adapter)
          ];
        };
      };
    };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/CodiePP/ekg-prometheus-adapter";
      rev = "1a258b6df7d9807d4c4ff3e99722223d31a2c320";
      sha256 = "16arrlxjkz9f8rd8v3l0yj70f2ij51didsxcz54jdv3j14pzmb5s";
      });
    }