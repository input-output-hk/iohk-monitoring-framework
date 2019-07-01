# Performance Test

## Purpose

The purpose of `example-performance` is to measure performance of `Switchboard`'s queue handling.

It sends predefined number (1000, 10000, 100000 and 1000000) of equal messages in the same backend and measures the time.

We use package [criterion](https://hackage.haskell.org/package/criterion) for benchmarking.

## Run

Run it using:

```
cabal new-run example-performance
```

or:

```
stack exec -- example-performance
```

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
