# iohk-monitoring-framework

[![Build status](https://badge.buildkite.com/1cc7939a1fed4972c15b8f87d510e0404b0eb65d73cfd1e30b.svg?branch=develop)](https://buildkite.com/input-output-hk/iohk-monitoring-framework)

This framework provides logging, benchmarking and monitoring.

## documentation

Documentation of the [source code](docs/IOHK-Monitoring-code.pdf) and
[tests](docs/IOHK-Monitoring-tests.pdf) are available in PDF format.

View our first presentation (2018-12-04) on this subject in [html](https://input-output-hk.github.io/iohk-monitoring-framework/pres-20181204/html/index.html)

## module dependencies

![Overview of modules](docs/OverviewModules.png)

## building

`cabal new-build`

`cabal new-test`

## examples

in directory `examples` are these examples:
* `simple`  -  run with `cabal new-exec example-simple`

## development

`ghcid -c "cabal new-repl"` watches for file changes and recompiles them immediately
