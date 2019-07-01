# iohk-monitoring-framework

[![Build status](https://badge.buildkite.com/1cc7939a1fed4972c15b8f87d510e0404b0eb65d73cfd1e30b.svg?branch=master)](https://buildkite.com/input-output-hk/iohk-monitoring-framework)
[![Coverage Status](https://coveralls.io/repos/github/input-output-hk/iohk-monitoring-framework/badge.svg?branch=master)](https://coveralls.io/github/input-output-hk/iohk-monitoring-framework?branch=master)

This framework provides logging, benchmarking and monitoring.

## documentation

Documentation of the [source code and tests](docs/IOHK-Monitoring.pdf) in PDF format. Please, download the PDF file and open in external viewer. It contains links that make it easy to navigate in the source code. Those links are not active in the online viewer.

Presentations and more documentation is available from our [docs](https://input-output-hk.github.io/iohk-monitoring-framework/) section.

## module dependencies

![Overview of modules](docs/OverviewModules.png)

## building and testing

`cabal new-build iohk-monitoring`

`cabal new-test pkg:iohk-monitoring:tests`

## examples
https://github.com/input-output-hk/iohk-monitoring-framework/edit/master/README.md
Some examples are available in the directory `examples`:
* `simple`  -  run with `cabal new-run example-simple`
* `complex`  -  run with `cabal new-run example-complex`
* `performance` - run with `cabal new-run example-performance`

These showcase the usage of this framework in an application. The *complex* example includes `EKGView` (http://localhost:12789) and the configuration editor (http://localhost:13789).

![Edit runtime configuration](docs/ConfigEditor.png)

### performance

The purpose of `example-performance` is to measure performance of `Switchboard`'s queue handling. It sends predefined number of equal messages in the same backend and measures the time.

Example output:

```
benchmarking 1000 objects
time                 10.79 ms   (9.616 ms .. 11.83 ms)
                     0.960 R²   (0.946 R² .. 0.976 R²)
mean                 7.915 ms   (7.442 ms .. 8.575 ms)
std dev              1.518 ms   (1.231 ms .. 1.982 ms)
variance introduced by outliers: 83% (severely inflated)

benchmarking 10000 objects
time                 118.1 ms   (114.1 ms .. 120.5 ms)
                     0.999 R²   (0.996 R² .. 1.000 R²)
mean                 112.2 ms   (109.3 ms .. 114.9 ms)
std dev              4.303 ms   (3.231 ms .. 5.514 ms)
variance introduced by outliers: 11% (moderately inflated)

benchmarking 100000 objects
time                 1.184 s    (1.138 s .. 1.229 s)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.135 s    (1.089 s .. 1.154 s)
std dev              32.52 ms   (9.455 ms .. 43.67 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking 1000000 objects
time                 14.84 s    (13.61 s .. 15.74 s)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 13.23 s    (11.59 s .. 13.86 s)
std dev              1.122 s    (3.664 ms .. 1.469 s)
variance introduced by outliers: 22% (moderately inflated)

```

## development

* `cabal new-build` and `cabal new-test`
* `ghcid -c "cabal new-repl"` watches for file changes and recompiles them immediately
* `liquid --ghc-option=-XOverloadedStrings --prune-unsorted src/Cardano/BM/*.lhs` verify top modules in iohk-monitoring using LiquidHaskell
