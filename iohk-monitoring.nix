{ mkDerivation, aeson, Cabal, array, async, auto-update, base, bytestring
, clock, containers, contravariant, directory, download, ekg, ekg-core
, filepath, katip, lens, mtl, process, QuickCheck, random
, safe-exceptions, semigroups, stdenv, stm, tasty, tasty-hunit
, tasty-quickcheck, template-haskell, text, time, time-units
, transformers, unix, unordered-containers, void, yaml
}:
mkDerivation {
  pname = "iohk-monitoring";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson Cabal array async auto-update base bytestring clock containers
    contravariant download directory ekg ekg-core filepath katip lens mtl
    safe-exceptions stm template-haskell text time time-units
    transformers unix unordered-containers yaml
  ];
  testHaskellDepends = [
    Cabal array base bytestring clock containers mtl process QuickCheck
    random semigroups stm tasty tasty-hunit tasty-quickcheck text time
    time-units transformers void
  ];
  description = "logging, benchmarking and monitoring framework";
  license = stdenv.lib.licenses.mit; #unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
