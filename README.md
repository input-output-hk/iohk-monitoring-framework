# iohk-monitoring-framework

[![Build status](https://badge.buildkite.com/1cc7939a1fed4972c15b8f87d510e0404b0eb65d73cfd1e30b.svg?branch=develop)](https://buildkite.com/input-output-hk/iohk-monitoring-framework)

This framework provides logging, benchmarking and monitoring.

## documentation

Documentation of the [source code and tests](docs/IOHK-Monitoring.pdf) in PDF format. Please, download the PDF file and open in external viewer. It contains links that make it easy to navigate in the source code. Those links are not active in the online viewer.

More documentation and the slides of our presentations are available in [html](https://input-output-hk.github.io/iohk-monitoring-framework/)

## module dependencies

![Overview of modules](docs/OverviewModules.png)

## building and testing

`cabal new-build all`

`cabal new-test all`

## examples
https://github.com/input-output-hk/iohk-monitoring-framework/edit/develop/README.md
Some examples are available in the directory `examples`:
* `simple`  -  run with `cabal new-run example-simple`
* `complex`  -  run with `cabal new-run example-complex`

These showcase the usage of this framework in an application. The *complex* example includes `EKGView` (http://localhost:12789) and the configuration editor (http://localhost:13789).

![Edit runtime configuration](docs/ConfigEditor.png)


## development

* `cabal new-build all` and `cabal new-test all`
* `ghcid -c "cabal new-repl"` watches for file changes and recompiles them immediately
