\documentclass[11pt,a4paper,twoside]{report}

\pagestyle{headings}

\usepackage{kpfonts}
\usepackage[margin=1in]{geometry}
\usepackage[pdfpagelabels,ocgcolorlinks]{hyperref}
\usepackage{todonotes}
\usepackage{amsmath}
\usepackage{mathtools}
\usepackage{colortbl}
\usepackage{verbatim}
\usepackage{graphicx}
\usepackage{pstricks}
\usepackage{fancyvrb}
\usepackage{wrapfig}
\usepackage{makeidx}
\makeindex

%include colorcode.fmt

%include local.fmt
%include references.fmt

\title{Cardano.BM - logging, benchmarking and monitoring}
\author{Alexander Diemand
  \and
        Andreas Triantafyllos}
\date{November 2018}

\begin{document}

\hypersetup{pageanchor=false}
\begin{titlepage}
\maketitle
\end{titlepage}

\begin{abstract}
This is a framework that combines logging, benchmarking and monitoring.
Complex evaluations of STM or monadic actions can be observed from outside
while reading operating system counters before and after, and calculating
their differences, thus relating resource usage to such actions.
Through interactive configuration, the runtime behaviour of logging or the
measurement of resource usage can be altered.
Further reduction in logging can be achieved by redirecting log messages to
an aggregation function which will output the running statistics with less
frequency than the original message.
\end{abstract}

\newpage

\hypersetup{pageanchor=true, linkcolor=olive}
\pagenumbering{arabic}

\tableofcontents

\newpage

\chapter{Logging, benchmarking and monitoring}

\section{Overview}

In figure \ref{fig:overview} we display the relationships among modules
in |Cardano.BM|. Central is the |Switchboard| (see Cardano.BM.Output.Switchboard)
that will redirect incoming log messages to selected backends according the
|Configuration| (see Cardano.BM.Configuration.Model). The log items are created
in the application's context and passed via a hierarchy of |Trace|s (see
Cardano.BM.Trace). Such a hierarchy can be built with the function |subTrace|.
The newly added child |Trace| will add its name to the logging context and
behave as configured. Among the different kinds of |Trace|s implemented are
|NoTrace| which suppresses all log items, |FilterTrace| which filters the
log items passing through it, and |ObservableTrace| which allows capturing of
operating system counters (see Cardano.BM.Data.SubTrace).
The backend |EKGView| (see Cardano.BM.Output.EKGView) displays selected values
in a browser. The |Log| backend is based on |katip| and outputs log items in
files or the console. The format can be chosen to be textual or JSON representation.
And finally, the |Aggregation| backend computes simple statistics over incoming
log items (e.g. last, min, max, mean, etc.) (see Cardano.BM.Data.Aggregated).

Output selection determines which log items of a named context are routed to
which backend. In the case of the |Log| output, this includes a configured
output sink (e.g. which file). Items that are aggregated lead to the creation
of an output of their current statistics. To prevent a potential infinite loop
these aggregation statistics cannot be routed again back into the |Aggregation|. 

With |Monitoring| we aim to shortcut the logging-analysis cycle and immediately
evaluate monitors on logged values when they become available. In case a monitor
is triggered a number of actions can be run: either internal actions that can
alter the |Configuration|, or actions that can lead to alerting in external
systems.

It is not the intention that this framework should (as part of normal use)
record sufficient information so as to make the sequence of events reproducible,
i.e. it is not an audit or transaction log.

\begin{figure}[ht]
\centering{
  \includegraphics[scale=0.48]{OverviewModules.pdf}
}
\caption{Overview of module relationships. The arrows indicate import of a module. The arrows with
a triangle at one end would signify "inheritance" in object-oriented programming,
but we use it to show that one module replaces the other in the namespace,
thus specializes its interface.}\label{fig:overview}
\end{figure}

\section{Requirements}

%include ../docs/requirements.tex

\section{Description}

%include ../docs/traces.tex

%include ../docs/mu-benchmarks.tex

%include ../docs/configuration.tex

%include ../docs/aggregation.tex

%include ../docs/output-selection.tex

%include ../docs/monitoring.tex

\section{Examples}

\subsection{Simple example showing plain logging}\label{sec:examplesimple}

%include ../examples/simple/Main.lhs

\subsection{Complex example showing logging, aggregation, and observing |IO| actions}\label{sec:examplecomplex}

%include ../examples/complex/Main.lhs

\section{Code listings}

%if style == newcode
\begin{code}
module Code
where

\end{code}
%endif

%include Cardano/BM/Observer/STM.lhs
%include Cardano/BM/Observer/Monadic.lhs

%include Cardano/BM/BaseTrace.lhs
%include Cardano/BM/Trace.lhs

%include Cardano/BM/Setup.lhs

%include Cardano/BM/Counters.lhs
%include Cardano/BM/Counters/Common.lhs
%include Cardano/BM/Counters/Dummy.lhs
%include Cardano/BM/Counters/Linux.lhs

%include Cardano/BM/Data/Aggregated.lhs
%include Cardano/BM/Data/AggregatedKind.lhs
%include Cardano/BM/Data/Backend.lhs
%include Cardano/BM/Data/BackendKind.lhs
%include Cardano/BM/Data/Configuration.lhs
%include Cardano/BM/Data/Counter.lhs
%include Cardano/BM/Data/LogItem.lhs
%include Cardano/BM/Data/Observable.lhs
%include Cardano/BM/Data/Output.lhs
%include Cardano/BM/Data/Rotation.lhs
%include Cardano/BM/Data/Severity.lhs
%include Cardano/BM/Data/SubTrace.lhs
%include Cardano/BM/Data/Trace.lhs

%include Cardano/BM/Configuration.lhs
%include Cardano/BM/Configuration/Model.lhs
%include Cardano/BM/Configuration/Static.lhs
%include Cardano/BM/Configuration/Editor.lhs

%include Cardano/BM/Output/Switchboard.lhs
%include Cardano/BM/Output/Log.lhs
%include Cardano/BM/Output/EKGView.lhs
%include Cardano/BM/Output/Aggregation.lhs
%include Cardano/BM/Output/Monitoring.lhs

\chapter{Testing}

%include ../test/Test.lhs

\printindex

\end{document}
