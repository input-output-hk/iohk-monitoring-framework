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

1 logging
=========

Purpose: capture and output messages

- message creation is hard-coded

- defines a ``Severity`` at call site

- decoupled: entered in queue; calling thread returns

- is routed in a statically programmed way

.. class:: substep
summary: quite static, transport-oriented, little performance price

.. class:: substep
What if we want to drop some of the messages that fill our logs?
Or, increase the ``Severity`` of another message which seems to be important?

.. note:

    without changing the code!

------

:data-scale: 3
:data-y: r1500

1.1 trace
=========

* implemented as a `contravariant` Trace (thanks to Alexander Vieth)

* we can build hierarchies: child traces forward to their parent

* each `Trace` has its own behaviour: `NoTrace` .. 

.. note:

      where a `covariant` (`F A -> F B`) produces a value `B`,

      a `contravariant` (`F B -> F A`) consumes it.

------

:data-scale: 1
:data-y: r-1500
:data-x: r5000

2 benchmarking
==============

Purpose: observe events and store them for later analysis

- record events with exact timestamp

- currently: formats as JSON and outputs directly to a file

.. class:: substep
- we will integrate this into our logging

.. class:: substep
What if we want to stop recording some of the events? Or, turn on others?

.. note:

    again, without changing the code!

------

:data-y: r0
:data-x: r5000

3 monitoring
============

Purpose: analyse messages and once above a threshold (frequency, value)
trigger a cascade of alarms

------

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

* aggregation

* filtering

------

:data-scale: 3
:data-y: r1500

4.1.2 EKG metrics view
======================

* defined standard metrics

* our own metrics: labels and gauges

------

:data-scale: 3
:data-y: r1500

4.1.3 Katip log files
=====================

* ``katip`` based queue and scribes

* log rotation

------

:data-y: r0
:data-x: r5000

5 requirements
==============

* Support

   * faces clients
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

6 performance and security considerations
=========================================

* how much does capturing of metrics cost?

* conditional compilation: can we exclude benchmarking code from end-user products?

------

7 integration
=============

* integration into ``node-shell``

* integration into ``ouroboros-network``

------

8 project overview
==================

* literate Haskell (thanks to Andres for `lhs2TeX`)

    * documentation of source code
    * documentation of tests

* some UML

* we still need help for:

    * ``nix`` scripts
    * ``buildkite`` CI setup


--> projectURL_

------

the end
=======

