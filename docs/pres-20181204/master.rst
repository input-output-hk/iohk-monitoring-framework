:title: iohk-monitoring-framework
:author: Alexander Diemand, Andreas Triantafyllos and Neil Davies
:description: logging, benchmarking, monitoring
:css: style.css

:data-transition-duration: 1500
:slide-numbers: false

.. _projectURL: https://github.com/input-output-hk/iohk-monitoring-framework

.. footer::

  IOHK - logging, benchmarking, monitoring @ https://github.com/input-output-hk/iohk-monitoring-framework


WIP logging, benchmarking and monitoring
========================================

Alexander Diemand, Andreas Triantafyllos, and Neil Davies
---------------------------------------------------------

2018-12-04
..........

------

Aim
===

- combine logging and benchmarking

- add monitoring on top

- runtime configuration

- adaptable to needs of various stakeholders

------

1 logging
=========

Purpose: capture and output messages

- static message creation ``(Severity, String)``

  - ``logInfo "this is a message"``

- fast - decoupled via message queue

- backend - routed in a statically programmed way

.. class:: substep

   What if we want to drop some of the messages?

   Or, increase the ``Severity`` of a particular message?

.. note:

    without changing the code!

------

:data-scale: 3
:data-y: r1500
:data-x: r750

1.1 trace
=========

- implemented as a Contravariant_ ``Trace`` (thanks to Alexander Vieth)

- not in monad stack, but passed as an argument

  - ``logInfo logTrace "this is a message"``

- New: we can build hierarchies, where child traces forward to their parent

- each `Trace` has its own behaviour: ``NoTrace`` .. 

  - example in: Trace.subTrace_

.. _Contravariant: https://hackage.haskell.org/package/contravariant-1.5/docs/Data-Functor-Contravariant.html

.. _Trace.subTrace: https://github.com/input-output-hk/iohk-monitoring-framework/blob/40eb8eb172037d85949f533efecfcffab54e136a/src/Cardano/BM/Trace.lhs#L296

.. note:

      where a `covariant` (`F A -> F B`) produces a value `B`,

      a `contravariant` (`F B -> F A`) consumes it.

------

:data-scale: 1
:data-y: r-1500
:data-x: r5000

2 benchmarking
==============

Purpose: observe events and prepare them for later analysis

- record events with exact timestamps

- currently: JSON format directly output to a file

.. class:: substep

- we will integrate this into our logging

.. class:: substep

What if we want to stop recording some of the events? Or, turn on others?

.. note:

    again, without changing the code!

------

:data-scale: 3
:data-y: r1500
:data-x: r750

2.1 observables
===============

- bracket ``STM`` (Observer.STM_) and ``Monad`` (Observer.Monad_) actions

- record O/S metrics: Counters.Linux_

.. _Counters.Linux: https://github.com/input-output-hk/iohk-monitoring-framework/blob/40eb8eb172037d85949f533efecfcffab54e136a/src/Cardano/BM/Counters/Linux.lhs#L36

.. _Observer.STM: https://github.com/input-output-hk/iohk-monitoring-framework/blob/40eb8eb172037d85949f533efecfcffab54e136a/src/Cardano/BM/Observer/STM.lhs#L31

.. _Observer.Monad: https://github.com/input-output-hk/iohk-monitoring-framework/blob/40eb8eb172037d85949f533efecfcffab54e136a/src/Cardano/BM/Observer/Monadic.lhs#L37

------

:data-scale: 1
:data-y: r-1500
:data-x: r5000

3 monitoring
============

Purpose: analyse messages and once above a threshold (frequency, value)
trigger a cascade of alarms

* <tbd>

------

:data-y: r0

4 configuration
===============

Purpose: change behaviour of LoBeMo in a named context

* changed at runtime

  * redirects output (output selection)
  * overwrites `Severity` (tbd)
  * filters by `Severity`
  * defines `SubTrace`

.. image:: ./ConfigurationModel.png

------

:data-scale: 3
:data-y: r1500

4.1 output selection
====================

Redirection of log messages and observables to different outputs:

aggregation | EKG | Katip

.. image:: ./Activity.png

------

:data-scale: 3
:data-y: r1500

4.1.1 information reduction
===========================

* Aggregation_

* filtering: traceConditionally_

.. _Aggregation: https://github.com/input-output-hk/iohk-monitoring-framework/blob/40eb8eb172037d85949f533efecfcffab54e136a/src/Cardano/BM/Aggregated.lhs#L13

.. _traceConditionally: https://github.com/input-output-hk/iohk-monitoring-framework/blob/40eb8eb172037d85949f533efecfcffab54e136a/src/Cardano/BM/Trace.lhs#L154

------

:data-y: r0
:data-x: r2200

4.1.2 EKG metrics view
======================

* defined standard metrics

* our own metrics: labels and gauges

------

4.1.3 Katip log files
=====================

* ``katip`` based queue and scribes

* log rotation

------

:data-scale: 1
:data-y: r-3000
:data-x: r5000

5 actual
========

- requirements

- performance & security

- integration, PoC

------

:data-scale: .75
:data-y: r500
:data-x: r1000

5.1 requirements
================


* Support

   * reduced size of logs
   * automated log analysis

* Devops

   * run *core* nodes
   * monitoring

* Developers

   * run unit/property testing
   * micro-benchmarks

* QA testing & benchmarks

   * run integration tests
   * run (holistic) benchmarks


.. note:

    usage-centric or user-centric?

------

:data-scale: .5
:data-y: r0
:data-x: r1000

5.2 performance and security considerations
===========================================

* how much does capturing of metrics cost?

* conditional compilation: can we exclude benchmarking code from end-user products?

------

:data-scale: .2
:data-y: r0
:data-x: r400

5.3 integration
===============

* integration into ``node-shell``

* PoC in ``ouroboros-network``

------

:data-scale: 1
:data-y: r-1500
:data-x: r3000

8 project overview
==================

    >> projectURL_ <<

* literate Haskell (thanks to Andres for `lhs2TeX`)

    * documentation of source code
    * documentation of tests

* this presentation_

* we still need help for:

    * ``nix`` scripts
    * ``buildkite`` CI setup


.. _presentation: https://input-output-hk.github.io/iohk-monitoring-framework/

------

the end
=======

