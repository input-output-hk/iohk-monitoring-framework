{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "libsystemd-journal"; version = "1.4.4"; };
      license = "BSD-3-Clause";
      copyright = "Oliver Charles (c) 2014";
      maintainer = "ollie@ocharles.org.uk";
      author = "Oliver Charles";
      homepage = "http://github.com/ocharles/libsystemd-journal";
      url = "";
      synopsis = "Haskell bindings to libsystemd-journal";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.pipes)
          (hsPkgs.pipes-safe)
          (hsPkgs.text)
          (hsPkgs.transformers)
          (hsPkgs.unix-bytestring)
          (hsPkgs.vector)
          (hsPkgs.uuid)
          (hsPkgs.unordered-containers)
          (hsPkgs.hashable)
          (hsPkgs.hsyslog)
          (hsPkgs.uniplate)
          (hsPkgs.semigroups)
          ];
        pkgconfig = [ (pkgconfPkgs."libsystemd") ];
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/ocharles/libsystemd-journal";
      rev = "16696812d6bf882fd348a54be9b573e9e115d2e3";
      sha256 = "09dyhy8cd22gyzw589jchpg0ixwr6iy4wl4jmm095rswga2ddrjf";
      });
    }