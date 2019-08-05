
# Documentation

In this directory, enter a `nix-shell` to have access to the required tools
to generate documentation. Then, simply run `make`. This will produce the
[IOHK-Monitoring.pdf](IOHK-Monitoring.pdf) from the source code.

# One-pagers

## Logging overview

A summary of [logging](one-pager/pdf/logging.pdf) on a single page.

A single-page description of [benchmarking](one-pager/pdf/benchmarking.pdf).

Expose of [monitoring](one-pager/pdf/monitoring.pdf).

# Presentations

## Presentation 2019-04-09

The second presentation with more focus on contravariant `Tracer` and how the switchboard routes messages to the backends.

[Presentation 2019-04-09](pres-20190409/pres-20190409_lobemo.pdf)

[Contravariant Functor](pres-20190409/contravariant-idea)


## Presentation 2018-12-04

This was the first presentation of `iohk-monitoring-framework`:

[Presentation 2018-12-04](pres-20181204/html/index.html)


# Haddock documentation

`Haddock` automatically generates documentation from source code:

[package contra-tracer](haddock/contra-tracer/index.html)

[package tracer-transformers](haddock/tracer-transformers/index.html)


## UML Diagrams

Diagrams have been edited with [BOUML](https://bouml.fr), a light-weight and fast UML editor.
License: "free of use", no license.

## Overview modules

![OverviewModules](OverviewModules.png)

## Components

![Components](Components.png)

## Activity

![Activity](Activity.png)

## STM observer

![STMObserver](STMObserver.png)

## Configuration

### configure output selection
![Output selection](Config_OutputSelection.png)

### configure severity filter
![Severity filter](Config_SeverityFilter.png)

### apply severity filter
![Application of Severity Filter](Trace_SeverityFilter.png)


## Setup procedure

![Setup procedure](SetupProcedure.png)

## Backend state machine

![Backend state machine](Backend_STM.png)


## Integration in Cardano.Shell

![CardanoShell components](CardanoShell_Components.png)

![CardanoShell sequence](CardanoShell_Seq.png)


