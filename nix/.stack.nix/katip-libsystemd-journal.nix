{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.8";
      identifier = { name = "katip-libsystemd-journal"; version = "0.0.0.0"; };
      license = "Apache-2.0";
      copyright = "Copyright (C) 2017 Nicolas Trangez";
      maintainer = "ikke@nicolast.be";
      author = "Nicolas Trangez";
      homepage = "https://github.com/haskell-service/katip-libsystemd-journal";
      url = "";
      synopsis = "A Katip scribe for systemd's journal";
      description = "This library provides some bridging code between the Katip logging API and\nsystemd's journal service, turning the latter into a target for application\nlogs.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.aeson)
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.hsyslog)
          (hsPkgs.katip)
          (hsPkgs.libsystemd-journal)
          (hsPkgs.template-haskell)
          (hsPkgs.text)
          (hsPkgs.unordered-containers)
          ];
        };
      tests = {
        "katip-libsystemd-journal-test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.katip)
            (hsPkgs.libsystemd-journal)
            (hsPkgs.pipes)
            (hsPkgs.pipes-safe)
            (hsPkgs.tasty)
            (hsPkgs.tasty-hspec)
            (hsPkgs.tasty-quickcheck)
            (hsPkgs.template-haskell)
            (hsPkgs.text)
            (hsPkgs.time)
            (hsPkgs.unix)
            (hsPkgs.unordered-containers)
            (hsPkgs.katip-libsystemd-journal)
            ];
          };
        };
      benchmarks = {
        "katip-libsystemd-journal-bench" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.criterion)
            (hsPkgs.deepseq)
            (hsPkgs.katip)
            (hsPkgs.libsystemd-journal)
            (hsPkgs.time)
            (hsPkgs.katip-libsystemd-journal)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/365andreas/katip-libsystemd-journal";
      rev = "5f8d074c617fd67e3bce377a06d22b908b8ab8d7";
      sha256 = "0sim2ag8ildz73bbrmm76qgdn6jr1r9ksdvkrlym95r0k36ra766";
      });
    }