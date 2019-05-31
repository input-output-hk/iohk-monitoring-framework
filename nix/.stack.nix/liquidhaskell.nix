{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {
      devel = false;
      include = false;
      deterministic-profiling = false;
      };
    package = {
      specVersion = "1.22";
      identifier = { name = "liquidhaskell"; version = "0.8.6.0"; };
      license = "BSD-3-Clause";
      copyright = "2010-19 Ranjit Jhala & Niki Vazou & Eric L. Seidel, University of California, San Diego.";
      maintainer = "Ranjit Jhala <jhala@cs.ucsd.edu>";
      author = "Ranjit Jhala, Niki Vazou, Eric Seidel";
      homepage = "https://github.com/ucsd-progsys/liquidhaskell";
      url = "";
      synopsis = "Liquid Types for Haskell";
      description = "Liquid Types for Haskell.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.Diff)
          (hsPkgs.aeson)
          (hsPkgs.binary)
          (hsPkgs.bytestring)
          (hsPkgs.cereal)
          (hsPkgs.cmdargs)
          (hsPkgs.containers)
          (hsPkgs.data-default)
          (hsPkgs.deepseq)
          (hsPkgs.directory)
          (hsPkgs.filepath)
          (hsPkgs.fingertree)
          (hsPkgs.ghc)
          (hsPkgs.ghc-boot)
          (hsPkgs.ghc-paths)
          (hsPkgs.ghc-prim)
          (hsPkgs.gitrev)
          (hsPkgs.hashable)
          (hsPkgs.hscolour)
          (hsPkgs.liquid-fixpoint)
          (hsPkgs.mtl)
          (hsPkgs.optparse-simple)
          (hsPkgs.parsec)
          (hsPkgs.pretty)
          (hsPkgs.syb)
          (hsPkgs.template-haskell)
          (hsPkgs.temporary)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.time)
          (hsPkgs.transformers)
          (hsPkgs.unordered-containers)
          (hsPkgs.vector)
          ];
        };
      exes = {
        "liquid" = { depends = [ (hsPkgs.base) (hsPkgs.liquidhaskell) ]; };
        "gradual" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cmdargs)
            (hsPkgs.hscolour)
            (hsPkgs.liquid-fixpoint)
            (hsPkgs.liquidhaskell)
            ];
          };
        "target" = {
          depends = [ (hsPkgs.base) (hsPkgs.hint) (hsPkgs.liquidhaskell) ];
          };
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.containers)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.mtl)
            (hsPkgs.optparse-applicative)
            (hsPkgs.process)
            (hsPkgs.stm)
            (hsPkgs.tagged)
            (hsPkgs.tasty)
            (hsPkgs.tasty-ant-xml)
            (hsPkgs.tasty-hunit)
            (hsPkgs.tasty-rerun)
            (hsPkgs.text)
            (hsPkgs.transformers)
            ];
          };
        "liquidhaskell-parser" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.liquid-fixpoint)
            (hsPkgs.liquidhaskell)
            (hsPkgs.parsec)
            (hsPkgs.syb)
            (hsPkgs.tasty)
            (hsPkgs.tasty-ant-xml)
            (hsPkgs.tasty-hunit)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/ucsd-progsys/liquidhaskell";
      rev = "46f11e8faef006e70d39572d08419283b1280b88";
      sha256 = "1vk2b451md14x7z40lqzxsfyc6lzmnz0i02n807al82gcah8ln4q";
      });
    }