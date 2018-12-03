:title: iohk-monitoring-framework
:author: Alexander Diemand, Andreas Triantafyllos and Neil Davis
:description: logging, benchmarking, monitoring
:css: style.css

.. _projectURL: https://github.com/input-output-hk/iohk-monitoring-framework

.. footer::

  IOHK - logging, benchmarking, monitoring @ https://github.com/input-output-hk/iohk-monitoring-framework


logging, benchmarking and monitoring
====================================

Alexander Diemand, Andreas Triantafyllos, and Neil Davis
--------------------------------------------------------

2018-12-04
..........

------

logging
=======

- message creation is hard-coded

- defines a ``Severity`` at call site

- decoupled: entered in queue; calling thread returns

- is routed in a statically programmed way

summary: quite static, transport-oriented, little performance price

What if we want to drop some of the messages that fill our logs?
Or, increase the ``Severity`` of another message which seems to be important?

.. note:

    without changing the code!

------

trace
=====

------

benchmarking
============

- record events with exact timestamp

- currently: formats as JSON and outputs directly to a file

summary: lightweight, not decoupled, duplication of logging effort

What if we want to stop recording some of the events?

.. note:

    again, without changing the code!

------

monitoring
==========


------

configuration
=============

* changed at runtime

  * redirects output (output selection)
  * overwrite `Severity`
  * defines `SubTrace`

.. image:: ./ConfigurationModel.png

------

output selection
================

Redirection of log messages and observables to different outputs
according configuration:

* aggregation

* EKG

* Katip

  * files
  * console

------

information reduction
=====================

* aggregation

* filtering

------

requirements
============

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

performance and security considerations
=======================================

------

project overview
================

* literate Haskell

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

