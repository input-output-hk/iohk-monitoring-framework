{
  extras = hackage:
    {
      packages = {
        "time-units" = (((hackage.time-units)."1.0.0").revisions).default;
        "ekg" = (((hackage.ekg)."0.4.0.15").revisions).default;
        "ekg-json" = (((hackage.ekg-json)."0.1.0.6").revisions).default;
        "prometheus" = (((hackage.prometheus)."2.1.1").revisions).default;
        "ekg-prometheus-adapter" = (((hackage.ekg-prometheus-adapter)."0.1.0.4").revisions).default;
        "process" = (((hackage.process)."1.6.5.0").revisions).default;
        "transformers" = (((hackage.transformers)."0.5.6.2").revisions).default;
        "containers" = (((hackage.containers)."0.5.11.0").revisions).default;
        "fgl-visualize" = (((hackage.fgl-visualize)."0.1.0.1").revisions).default;
        "located-base" = (((hackage.located-base)."0.1.1.1").revisions).default;
        } // {
        contra-tracer = ./contra-tracer.nix;
        iohk-monitoring = ./iohk-monitoring.nix;
        liquidhaskell = ./liquidhaskell.nix;
        liquid-fixpoint = ./liquid-fixpoint.nix;
        katip-libsystemd-journal = ./katip-libsystemd-journal.nix;
        libsystemd-journal = ./libsystemd-journal.nix;
        };
      compiler.version = "8.6.4";
      compiler.nix-name = "ghc864";
      };
  resolver = "lts-13.15";
  compiler = "ghc-8.6.4";
  }