{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { devel = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "liquid-fixpoint"; version = "0.8.0.2"; };
      license = "BSD-3-Clause";
      copyright = "2010-17 Ranjit Jhala, University of California, San Diego.";
      maintainer = "jhala@cs.ucsd.edu";
      author = "Ranjit Jhala, Niki Vazou, Eric Seidel";
      homepage = "https://github.com/ucsd-progsys/liquid-fixpoint";
      url = "";
      synopsis = "Predicate Abstraction-based Horn-Clause/Implication Constraint Solver";
      description = "This package implements an SMTLIB based Horn-Clause/Logical\nImplication constraint solver used for Liquid Types.\n\nThe package includes:\n\n1. Types for Expressions, Predicates, Constraints, Solutions\n\n2. Code for solving constraints\n\nRequirements\n\nIn addition to the .cabal dependencies you require\n\n- A Z3 (<http://z3.codeplex.com>) or CVC4 (<http://cvc4.cs.nyu.edu>) binary.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.array)
          (hsPkgs.async)
          (hsPkgs.attoparsec)
          (hsPkgs.syb)
          (hsPkgs.cmdargs)
          (hsPkgs.ansi-terminal)
          (hsPkgs.bifunctors)
          (hsPkgs.binary)
          (hsPkgs.bytestring)
          (hsPkgs.containers)
          (hsPkgs.deepseq)
          (hsPkgs.directory)
          (hsPkgs.filemanip)
          (hsPkgs.filepath)
          (hsPkgs.ghc-prim)
          (hsPkgs.intern)
          (hsPkgs.mtl)
          (hsPkgs.parsec)
          (hsPkgs.pretty)
          (hsPkgs.boxes)
          (hsPkgs.parallel)
          (hsPkgs.process)
          (hsPkgs.syb)
          (hsPkgs.text)
          (hsPkgs.transformers)
          (hsPkgs.hashable)
          (hsPkgs.unordered-containers)
          (hsPkgs.cereal)
          (hsPkgs.text-format)
          (hsPkgs.fgl)
          (hsPkgs.fgl-visualize)
          (hsPkgs.dotgen)
          (hsPkgs.time)
          (hsPkgs.parallel-io)
          (hsPkgs.located-base)
          ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs.ascii-progress);
        };
      exes = {
        "fixpoint" = { depends = [ (hsPkgs.base) (hsPkgs.liquid-fixpoint) ]; };
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.process)
            (hsPkgs.stm)
            (hsPkgs.containers)
            (hsPkgs.mtl)
            (hsPkgs.transformers)
            (hsPkgs.tasty)
            (hsPkgs.tasty-hunit)
            (hsPkgs.tasty-rerun)
            (hsPkgs.tasty-ant-xml)
            (hsPkgs.tasty-hunit)
            (hsPkgs.text)
            ];
          };
        "testparser" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.tasty)
            (hsPkgs.tasty-hunit)
            (hsPkgs.tasty-rerun)
            (hsPkgs.tasty-ant-xml)
            (hsPkgs.tasty-hunit)
            (hsPkgs.text)
            (hsPkgs.liquid-fixpoint)
            ] ++ (if flags.devel
            then [
              (hsPkgs.array)
              (hsPkgs.async)
              (hsPkgs.attoparsec)
              (hsPkgs.syb)
              (hsPkgs.cmdargs)
              (hsPkgs.ansi-terminal)
              (hsPkgs.bifunctors)
              (hsPkgs.binary)
              (hsPkgs.bytestring)
              (hsPkgs.containers)
              (hsPkgs.deepseq)
              (hsPkgs.directory)
              (hsPkgs.filemanip)
              (hsPkgs.filepath)
              (hsPkgs.ghc-prim)
              (hsPkgs.intern)
              (hsPkgs.mtl)
              (hsPkgs.parsec)
              (hsPkgs.pretty)
              (hsPkgs.boxes)
              (hsPkgs.parallel)
              (hsPkgs.process)
              (hsPkgs.syb)
              (hsPkgs.text)
              (hsPkgs.transformers)
              (hsPkgs.hashable)
              (hsPkgs.unordered-containers)
              (hsPkgs.cereal)
              (hsPkgs.text-format)
              (hsPkgs.fgl)
              (hsPkgs.fgl-visualize)
              (hsPkgs.dotgen)
              (hsPkgs.time)
              ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "7.10.2") (hsPkgs.located-base)
            else [ (hsPkgs.liquid-fixpoint) ]);
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/ucsd-progsys/liquid-fixpoint";
      rev = "42c027ab9ae47907c588a2f1f9c05a5e0aa881e9";
      sha256 = "17qmzq1vx7h04yd38drr6sh6hys3q2rz62qh3pna9kbxlcnikkqf";
      });
    }