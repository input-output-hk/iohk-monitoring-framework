{
  extras = hackage:
    {
      packages = {
        "time-units" = (((hackage.time-units)."1.0.0").revisions).default;
        "ekg" = (((hackage.ekg)."0.4.0.15").revisions).default;
        "ekg-json" = (((hackage.ekg-json)."0.1.0.6").revisions).default;
        "process" = (((hackage.process)."1.6.5.0").revisions).default;
        "transformers" = (((hackage.transformers)."0.5.6.2").revisions).default;
        } // {
        contra-tracer = ./contra-tracer.nix;
        iohk-monitoring = ./iohk-monitoring.nix;
        };
      compiler.version = "8.6.4";
      compiler.nix-name = "ghc864";
      };
  resolver = "lts-13.15";
  compiler = "ghc-8.6.4";
  }