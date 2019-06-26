# Milestone planning

(see [Github milestones](https://github.com/input-output-hk/iohk-monitoring-framework/milestones?direction=asc&sort=title&state=open))

|     | Id  | depends on | title | description | deliverable |
|-----|-----|------------|-------|-------------|-------------|
| [_] |[**LOG001**](https://github.com/input-output-hk/iohk-monitoring-framework/milestone/22) |          | Trace point API design | [ ] review | Other teams who will use API validate design, sign off | API design doc, tests |
| [_] | [E1.1](https://github.com/input-output-hk/iohk-monitoring-framework/issues/246)   |        | API documentation and tests | -> contra-tracer | API doc, tests |
| [_] | T1.1.1 |        | write API documentation     | format: haddock? | API doc |
| [_] | T1.1.2 | T1.1.1 | implement API tests         |  | API tests |
| [x] | T1.1.3 | T1.1.1 | implement API examples      |  | API tests |
| [_] | [E1.2](https://github.com/input-output-hk/iohk-monitoring-framework/issues/247)   |        | Signoff                     |  | signoff |
| [_] | T1.2.1 | T1.1.3 | Identify who will sign off  |  | signer |
| [_] | T1.2.2 | T1.2.1 | get sign off                |  | signoff |

|     | Id  | depends on | title | description | deliverable |
|-----|-----|------------|-------|-------------|-------------|
| [_] | [**LOG002**](https://github.com/input-output-hk/iohk-monitoring-framework/milestone/21) |  | Trace point API integration with network component  | API being used partially in 2 components - Network and Ledger  | integration of logging and benchmarking into networking (block fetch logic) |
| [_] | [E2.1](https://github.com/input-output-hk/iohk-monitoring-framework/issues/274)   |        | microbenchmarking |  | micro-benchmarks |
| [_] | T2.1.1 |        | define functions to be benchmarked | -> Duncan | requirements |
| [_] | T2.1.2 | T2.1.1 | bracket function 'f1' in module 'Q' | <<< tbd | implementation |
| [_] | T2.1.3 | T2.1.1 | bracket function 'f2' in module 'R' | <<< tbd | implementation |
| [_] | T2.1.4 | T2.1.3 | post-processing of captured observables |  | statistics |
| [_] | [E2.2](https://github.com/input-output-hk/iohk-monitoring-framework/issues/277)   | E2.1   | structured logging |  | structured logging |
| [_] | T2.2.1 |        | define types to be logged |  | requirements |
| [_] | T2.2.2 | T2.2.1 | implement 'ToObject' instances |  | implementation |
| [_] | [E2.3](https://github.com/input-output-hk/iohk-monitoring-framework/issues/281)   | E2.1   | monitoring |  |  |
| [_] | T2.3.1 |        | requirements for monitoring values, display, alerting |  | requirements |
| [_] | T2.3.2 | T2.3.1 | implement routing, aggregation, monitor |  | implementation |
| [_] | T2.3.3 | T2.3.2 | alerting |  | alerting |

|     | Id  | depends on | title | description | deliverable |
|-----|-----|------------|-------|-------------|-------------|
| [_] | [**LOG003**](https://github.com/input-output-hk/iohk-monitoring-framework/milestone/18) |  | Trace point API integration with ledger component  |  | integration of logging and benchmarking into 'cardano-ledger' |
| [_] | |  |   Microbenchmarking (action timing support) for ledger batch mode validation  |  |  |
| [_] | |  |   Structured log output to file from ledger batch mode validation  |  |  |
| [_] | |  |   EKG Monitoring Output from ledger batch mode validation  | UTxO size would be an interesting test output |  |
| [x] | [E3.1](https://github.com/input-output-hk/iohk-monitoring-framework/issues/231)   |        | microbenchmarking |  | micro-benchmarks |
| [x] | T3.1.1 |        | define functions to be benchmarked |  | requirements |
| [x] | T3.1.2 | T3.1.1 | bracket function 'f1' in module 'A' | UTxO size would be an interesting test output | implementation |
| [x] | T3.1.3 | T3.1.1 | bracket function 'f2' in module 'B' | <<< tbd | implementation |
| [x] | T3.1.4 | T3.1.3 | post-processing of captured observables |  | statistics |
| [_] | [E3.2](https://github.com/input-output-hk/iohk-monitoring-framework/issues/234)   | E3.1   | structured logging |  | structured logging |
| [_] | T3.2.1 |        | define types to be logged |  | requirements |
| [_] | T3.2.2 | T3.2.1 | implement 'ToObject' instances |  | implementation |
| [x] | [E3.3](https://github.com/input-output-hk/iohk-monitoring-framework/issues/238)   | E3.2   | monitoring |  | monitoring |
| [x] | T3.3.1 |        | requirements for monitoring values, display, alerting |  | requirements |
| [x] | T3.3.2 | T3.3.1 | implement routing, aggregation, monitor, display | display: EKG | implementation |
| [x] | T3.3.3 | T3.3.2 | alerting |  | alerting |

|     | Id  | depends on | title | description | deliverable |
|-----|-----|------------|-------|-------------|-------------|
| [_] | [**LOG004**](https://github.com/input-output-hk/iohk-monitoring-framework/milestone/23) |  | Trace point API integration with Wallet BE | Trace point API integration with Wallet BE |  |
| [_] | [E4.1](https://github.com/input-output-hk/iohk-monitoring-framework/issues/328)   |        | microbenchmarking |  | micro-benchmarks |
| [_] | T4.1.1 |        | define functions to be benchmarked |  | requirements |
| [_] | T4.1.2 | T4.1.1 | bracket function 'f1' in module 'Performing' | <<< tbd | implementation |
| [_] | T4.1.3 | T4.1.1 | bracket function 'f2' in module 'Utils' | <<< tbd | implementation |
| [_] | T4.1.4 | T4.1.3 | post-processing of captured observables |  | statistics |
| [_] | [E4.2](https://github.com/input-output-hk/iohk-monitoring-framework/issues/331)   | E4.1   | structured logging |  | structured logging |
| [_] | T4.2.1 |        | define types to be logged |  | requirements |
| [_] | T4.2.2 | T4.2.1 | implement 'ToObject' instances |  | implementation |
| [_] | [E4.3](https://github.com/input-output-hk/iohk-monitoring-framework/issues/335)   | E4.1   | monitoring |  | monitoring |
| [_] | T4.3.1 |        | requirements for monitoring values, display, alerting |  | requirements |
| [_] | T4.3.2 | T4.3.1 | implement routing, aggregation, monitor, display | display: EKG | implementation |
| [_] | T4.3.3 | T4.3.2 | alerting |  | alerting |

|     | Id  | depends on | title | description | deliverable |
|-----|-----|------------|-------|-------------|-------------|
| [_] | [**LOG005**](https://github.com/input-output-hk/iohk-monitoring-framework/milestone/16) |  |  | Tracer BE switchboard complete - Performance related  | No current work planned for this | 
| [_] |  |  | depends on requirements |  | 
| [_] |  |  | bridge Tracer to Switchboard (aggregation, monitoring, ..) |  | 


|     | Id  | depends on | title | description | deliverable |
|-----|-----|------------|-------|-------------|-------------|
| [_] | [**LOG006**](https://github.com/input-output-hk/iohk-monitoring-framework/milestone/13) |  | Reenable benchmarks on new code |  | compatible benchmarks (compared to cardano-sl) |
| [_] |  |  |   Full coverage in wallet BE - Need to align GitHub naming | Tracing coverage of all major components in the | wallet BE |  |
| [_] |  |  |   Full coverage in node - Need to align GitHub naming | Tracing coverage of all major components in the node |  |
| [_] | [E6.1](https://github.com/input-output-hk/iohk-monitoring-framework/issues/322)   | LOG003, LOG004 | specification of benchmarks |  | specification |
| [_] | T6.1.1 |        | re-engineer measurements in cardano-sl | comparable measurement points should lead to comparable | benchmarks | specification of measurements |
| [_] | T6.1.2 | T6.1.1 | specification of bm metrics (KPI) |  | specification of analysis |
| [_] | [E6.2](https://github.com/input-output-hk/iohk-monitoring-framework/issues/323)   | E6.1   | implementation |  | implementation |
| [_] | T6.2.1 | T6.1.2 | implement points of measurement | block creation times; probably in cardano-ledger (was cardano-db) | | implementation |
| [_] | T6.2.2 | T6.1.2 | implement points of measurement | mempool status; probably in cardano-ledger (was cardano-db) | | implementation |
| [_] | T6.2.3 | T6.1.2 | implement points of measurement | networking? | implementation |
| [_] | T6.2.4 | T6.1.2 | implement points of measurement | wallet? | implementation |
| [_] | T6.2.5 | T6.2.4 | adapt 'iohk-ops' | cluster management | implementation |
| [_] | [E6.3](https://github.com/input-output-hk/iohk-monitoring-framework/issues/324)   | E6.2   | analysis and reporting |  | analysis and report |
| [_] | T6.3.1 |        | adapt 'post-mortem' tool | or replace/eliminate? | analysis |
| [_] | T6.3.2 |        | re-make R reporting |  | report |

|     | Id  | depends on | title | description | deliverable |
|-----|-----|------------|-------|-------------|-------------|
| [_] | [**LOG007**](https://github.com/input-output-hk/iohk-monitoring-framework/milestone/24) |  |  | First sign off from DevOps | Output is in a form they can consume; also address system specific | [ ] questions apart from log output | validation of output format conforming to specification |
| [x] | [E7.1](https://github.com/input-output-hk/iohk-monitoring-framework/issues/239)   |        | DevOps' requirements |  | requirements |
| [x] | T7.1.1 |        | DevOps' requirements | trace points (essential); involve Neil | requirements |
| [_] | [E7.2](https://github.com/input-output-hk/iohk-monitoring-framework/issues/240)   |        | implementation |  | implementation |
| [x] | T7.2.1 | T7.1.1 | implementation of log output | conforming to spec | implementation |
| [x] | T7.2.2 | T7.2.1 | implementation of log backend | statsd' scribe | implementation |
| [x] | T7.2.3 | T7.2.2 | implementation of validating test | >> add to 'CI' | validating test |
| [_] | T7.2.4 | T7.2.3 | signoff by Devops |  | signoff |

|     | Id  | depends on | title | description | deliverable |
|-----|-----|------------|-------|-------------|-------------|
| [_] | [**LOG008**](https://github.com/input-output-hk/iohk-monitoring-framework/milestone/17) | LOG006 |  | benchmarking on Shelley |  |  |
| [_] | [E8.1](https://github.com/input-output-hk/iohk-monitoring-framework/issues/366)   |        | specification of benchmarks |  | specification |
| [_] | T8.1.1 |        | specification of bm metrics (KPI) |  | specification of measurements |
| [_] | [E8.2](https://github.com/input-output-hk/iohk-monitoring-framework/issues/371)   |        | implementation |  | implementation |
| [_] | T8.2.1 | T8.1.1 | implement points of measurement | <<< tbd | implementation |
| [_] | T8.2.2 | T8.1.1 | implement points of measurement | <<< tbd | implementation |
| [_] | T8.2.3 | T8.1.1 | implement points of measurement | topology? | implementation |
| [_] | T8.2.4 | T8.2.3 | adapt 'iohk-ops' | cluster management | implementation |
| [_] | [E8.3](https://github.com/input-output-hk/iohk-monitoring-framework/issues/374)   | E8.2   | analysis and reporting |  | analysis and report |
| [_] | T8.3.1 |        | adapt 'post-mortem' tool |  | analysis |
| [_] | T8.3.2 |        | adapt R reporting |  | report |

|       | Id  | depends on | title | description | deliverable |
|-------|-----|------------|-------|-------------|-------------|
| [_] | [**LOG009**](https://github.com/input-output-hk/iohk-monitoring-framework/milestone/25) |  |  | Trace point API integration with consensus |  |  |
| [_] | [E9.1](https://github.com/input-output-hk/iohk-monitoring-framework/issues/319)   |        | microbenchmarking |  | micro-benchmarks |
| [_] | T9.1.1 |        | define functions to be benchmarked | -> Duncan | requirements |
| [_] | T9.1.2 | T9.1.1 | bracket function 'f1' in module 'V' | <<< tbd | implementation |
| [_] | T9.1.3 | T9.1.1 | bracket function 'f2' in module 'W' | <<< tbd | implementation |
| [_] | T9.1.4 | T9.1.3 | post-processing of captured observables |  | statistics |
| [_] | [E9.2](https://github.com/input-output-hk/iohk-monitoring-framework/issues/320)   | E9.1   | structured logging |  | structured logging |
| [_] | T9.2.1 |        | define types to be logged |  | requirements |
| [_] | T9.2.2 | T9.2.1 | implement 'ToObject' instances |  | implementation |
| [_] | [E9.3](https://github.com/input-output-hk/iohk-monitoring-framework/issues/321)   | E9.1   | monitoring |  |  |
| [_] | T9.3.1 |        | requirements for monitoring values, display, alerting |  | requirements |
| [_] | T9.3.2 | T9.3.1 | implement routing, aggregation, monitor |  | implementation |
| [_] | T9.3.3 | T9.3.2 | alerting |  | alerting |
