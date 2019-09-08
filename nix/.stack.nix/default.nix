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
        "libsystemd-journal" = (((hackage.libsystemd-journal)."1.4.4").revisions).default;
        } // {
        iohk-monitoring = ./iohk-monitoring.nix;
        contra-tracer = ./contra-tracer.nix;
        };
      compiler.version = "8.6.5";
      compiler.nix-name = "ghc865";
      };
  resolver = "lts-13.26";
  compiler = "ghc-8.6.5";
  }