{
  extras = hackage:
    {
      packages = {
        "time-units" = (((hackage.time-units)."1.0.0").revisions).default;
        "libsystemd-journal" = (((hackage.libsystemd-journal)."1.4.4").revisions).default;
        "katip" = (((hackage.katip)."0.8.3.0").revisions).default;
        } // {
        contra-tracer = ./contra-tracer.nix;
        tracer-transformers = ./tracer-transformers.nix;
        iohk-monitoring = ./iohk-monitoring.nix;
        lobemo-backend-aggregation = ./lobemo-backend-aggregation.nix;
        lobemo-backend-editor = ./lobemo-backend-editor.nix;
        lobemo-backend-ekg = ./lobemo-backend-ekg.nix;
        lobemo-backend-graylog = ./lobemo-backend-graylog.nix;
        lobemo-backend-monitoring = ./lobemo-backend-monitoring.nix;
        lobemo-backend-trace-acceptor = ./lobemo-backend-trace-acceptor.nix;
        lobemo-backend-trace-forwarder = ./lobemo-backend-trace-forwarder.nix;
        lobemo-scribe-systemd = ./lobemo-scribe-systemd.nix;
        lobemo-examples = ./lobemo-examples.nix;
        };
      compiler.version = "8.6.5";
      compiler.nix-name = "ghc865";
      };
  resolver = "lts-13.26";
  compiler = "ghc-8.6.5";
  }