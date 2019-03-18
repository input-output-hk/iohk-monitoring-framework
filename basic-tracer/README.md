
Tracer
======


This is a minimalistic implementation of a contravariant `Tracer`
used as the frontend for logging, benchmarking and monitoring.

Events can be traced to a hierarchy of tracers. The last transformer
decides where the trace ends up: on console or the switchboard of the
logging subsystem for further processing.

