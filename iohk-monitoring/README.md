# iohk-monitoring-framework

[![Build status](https://badge.buildkite.com/1cc7939a1fed4972c15b8f87d510e0404b0eb65d73cfd1e30b.svg?branch=develop)](https://buildkite.com/input-output-hk/iohk-monitoring-framework)

This framework provides logging, benchmarking and monitoring.

## documentation

Documentation of the [source code and tests](docs/IOHK-Monitoring.pdf) in PDF format. Please, download the PDF file and open in external viewer. It contains links that make it easy to navigate in the source code. Those links are not active in the online viewer.

View our first presentation (2018-12-04) on this subject in [html](https://input-output-hk.github.io/iohk-monitoring-framework/pres-20181204/html/index.html)

## module dependencies

![Overview of modules](docs/OverviewModules.png)

## building and testing

`cabal new-build iohk-monitoring`

`cabal new-test pkg:iohk-monitoring:tests`

## examples
https://github.com/input-output-hk/iohk-monitoring-framework/edit/develop/README.md
Some examples are available in the directory `examples`:
* `simple`  -  run with `cabal new-run example-simple`
* `complex`  -  run with `cabal new-run example-complex`

These showcase the usage of this framework in an application. The *complex* example includes `EKGView` (http://localhost:12789) and the configuration editor (http://localhost:13789).

![Edit runtime configuration](docs/ConfigEditor.png)


## development

* `cabal new-build` and `cabal new-test`
* `ghcid -c "cabal new-repl"` watches for file changes and recompiles them immediately
