{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "contra-tracer"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "Alexander Vieth";
      homepage = "";
      url = "";
      synopsis = "A simple interface for logging, tracing and monitoring";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.5") (hsPkgs.contravariant);
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/contra-tracer";
      rev = "6c3b3072b8e3632619516a75f7309825118fc78e";
      sha256 = "1nn3v53chvl7zm35bb6jig2zlgc7ngz6ix6yzslrqq0amg1sq6mz";
      });
    }