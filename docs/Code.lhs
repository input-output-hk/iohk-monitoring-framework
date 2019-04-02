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
\usepackage{listings}
\usepackage{graphicx}
\usepackage{pstricks}
\usepackage{fancyvrb}
\usepackage{wrapfig}
\usepackage{makeidx}

\usepackage{tikz}
\usetikzlibrary{shapes,arrows,positioning}
\usetikzlibrary{calc,decorations.markings}

\lstnewenvironment{hsl}{\lstset{language=Haskell,basicstyle=\small,xleftmargin=16pt}}{}

\makeindex

%include colorcode.fmt

%include local.fmt
%include references.fmt

\title{Cardano.BM - logging, benchmarking and monitoring}
\author{Alexander Diemand
  \and
        Andreas Triantafyllos}
\date{April 2019}

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

\section{Main concepts}

These are main concepts of the framework:

\begin{enumerate}
  \item LogObject
  \item Trace
  \item Backend
  \item Configuration
\end{enumerate}

Let's describe each of them.

\subsection{LogObject}

|LogObject| is a message. It contains a logger name, meta information
(such as timestamp or severity level) and some particular message:

\begin{figure}[ht]
\centering{
\begin{tikzpicture}
\node[ draw
     , fill=blue!14
     , text depth=1.8cm
     , minimum width=4.6cm
     , minimum height=3.6cm
     ] (logObj)
     {LogObject};
\node[ text width=4cm
     ] (logObj)
     {LogObject};
\node[ draw
     , text width=4cm
     , fill=yellow!15
     ] (who)
     {Who sent message};
\node[ draw
     , text width=4cm
     , fill=orange!15
     , below=0cm of who
     ] (meta)
     {Info about message};
\node[ draw
     , text width=4cm
     , fill=green!15
     , below=0cm of meta
     ] {Message itself};
\end{tikzpicture}
}
\end{figure}

Please see |Cardano.BM.Data.LogItem| for more details.

\subsection{Trace}

You can think of |Trace| as a pipeline for messages. It is a \textit{consumer} of
messages from user's point of view and a \textit{source} of messages from framework's
point of view.

\tikzstyle{block} = [ rectangle
                    , draw
                    , text centered
                    , minimum height = 2em
                    ]
\tikzstyle{brown}  = [fill = brown!20]
\tikzstyle{red}    = [fill = red!15]
\tikzstyle{green}  = [fill = green!15]
\tikzstyle{blue}   = [fill = blue!14]
\tikzstyle{line}   = [draw, -latex']
\tikzstyle{dline}  = [draw, line width = 0.4mm, dotted]

\begin{center}
\begin{tikzpicture}
% Place nodes
\node [block, red]                                        (user)     {User};
\node [block, blue,  below = 1cm of user]                 (m1)       {$m_2$};
\node [block, green, below = 0.5cm of m1]                 (trace)    {Trace};
\node [              left  = 3.5cm of trace]              (lb)       { };
\node [              below right = 0.1cm and 0.1cm of lb] (label)    {Framework};
\node [              right = 3.5cm of trace]              (rb)       { };
\node [block, blue,  below = 0.5cm of trace]              (m2)       {$m_1$};
\node [block, brown, below = 1cm of m2]                   (handler)  {Some handler};
% Draw edges
\path [line]  (user)  -- (m1);
\path [line]  (m1)    -- (trace);
\path [dline] (trace) -- (lb);
\path [dline] (trace) -- (rb);
\path [line]  (trace) -- (m2);
\path [line]  (m2)    -- (handler);
\end{tikzpicture}
\end{center}

Please see the section \ref{contravariantfunctors} for more details about
ideas behind the |Trace|.

\subsection{Backend}

|Backend| is something that can process incoming messages.

The central backend is a |Switchboard|. It sets up other backends and
redirects incoming messages to these backends according to configuration:

\begin{figure}[ht]
\tikzstyle{block} = [ rectangle
                    , draw
                    , text centered
                    , minimum height = 2em
                    ]
\tikzstyle{brown}  = [fill = brown!20]
\tikzstyle{blue}   = [fill = blue!14]
\tikzstyle{line}   = [draw, -latex']

\centering{
\begin{tikzpicture}
% Place nodes
\node                                                       (nextM)  {\ldots};
\node [block, blue,  below = 0.5cm of nextM]                (m5)     {$m_5$};
\node [block, blue,  below = 0.5cm of m5]                   (m4)     {$m_4$};
\node [block, brown, below = 1cm of m4]                     (switch) {Switchboard};
\node [block, blue,  below left = 1cm and 1cm of switch]    (m1)     {$m_1$};
\node [block, blue,  below = 1cm of switch]                 (m2)     {$m_3$};
\node [block, blue,  below right = 1cm and 1cm of switch]   (m3)     {$m_2$};
\node [block, brown, below = 1cm of m1]                     (be1)    {$Backend_A$};
\node [block, brown, below = 1cm of m2]                     (be2)    {$Backend_B$};
\node [              right of = be2, node distance = 1.3cm] (etc)    {\ldots};
\node [block, brown, below = 1cm of m3]                     (beN)    {$Backend_N$};
% Draw edges
\path [line] (nextM)  -- (m5);
\path [line] (m5)     -- (m4);
\path [line] (m4)     -- (switch);
\path [line] (switch) -- (m1);
\path [line] (switch) -- (m2);
\path [line] (switch) -- (m3);
\path [line] (m1)     -- (be1);
\path [line] (m2)     -- (be2);
\path [line] (m3)     -- (beN);
\end{tikzpicture}
}
\end{figure}

Please see |Cardano.BM.Data.Backend| and |Cardano.BM.Output.Switchboard| for
more details.

\subsection{Configuration}

|Configuration| describes how framework parts work together. It can be parsed from
the YAML-file or can be defined explicitly as a |Configuration|.

Please note that configuration can be changed at the runtime using interactive
editor, see |Cardano.BM.Configuration.Editor| for more details.

\section{Overview}

In figure \ref{fig:overview} we display the relationships among modules
in |Cardano.BM|.

\subsection{Backends}

As was mentioned earlier, central backend is the |Switchboard| (see |Cardano.BM.Output.Switchboard|)
that redirects incoming log messages to selected backends according to
|Configuration| (see |Cardano.BM.Configuration.Model|).

The backend |EKGView| (see |Cardano.BM.Output.EKGView|) displays selected values
in a browser.

The |Log| backend (see |Cardano.BM.Output.Log|) is based on \href{http://hackage.haskell.org/package/katip}{katip}
package and outputs log items in files or in the console. The format can be chosen to be textual or JSON representation.

The |Aggregation| backend (see |Cardano.BM.Output.Aggregation|) computes simple statistics over incoming
log items (e.g. last, min, max, mean, etc.) (see |Cardano.BM.Data.Aggregated| as well).

The |LogBuffer| backend (see |Cardano.BM.Output.LogBuffer|) collects the latest messages
from other backends and shows these collected messages via GUI.

Output selection determines which log items of a named context are routed to
which backend. In the case of the |Log| output, this includes a configured
output sink (e.g. which file). Items that are aggregated lead to the creation
of an output of their current statistics. To prevent a potential infinite loop
these aggregation statistics cannot be routed again back into the |Aggregation|.

\subsection{Trace}

The log items are created in the application's context and passed via a hierarchy
of |Trace|s (see |Cardano.BM.Trace|). Such a hierarchy can be built with the function
|setSubTrace|. The newly added child |Trace| will add its name to the logging context and
behave as configured. Among the different kinds of |Trace|s implemented are:

\begin{enumerate}
  \item |NoTrace| which suppresses all log items,
  \item |FilterTrace| which filters the log items passing through it,
  \item |ObservableTrace| which allows capturing of operating system counters (see |Cardano.BM.Data.SubTrace|).
\end{enumerate}

\subsection{Monitoring}

With |Monitoring| we aim to shortcut the logging-analysis cycle and immediately
evaluate monitors on logged values when they become available. In case a monitor
is triggered a number of actions can be run: either internal actions that can
alter the |Configuration|, or actions that can lead to alerting in external
systems.

\subsection{IMPORTANT!}

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

%include requirements.tex

\section{Description}

%include contravariant.tex

%include traces.tex

%include mu-benchmarks.tex

%include configuration.tex

%include aggregation.tex

%include output-selection.tex

%include monitoring.tex

\section{Examples}

\subsection{Simple example showing plain logging}\label{sec:examplesimple}

%include ../iohk-monitoring/examples/simple/Main.lhs

\subsection{Complex example showing logging, aggregation, and observing |IO| actions}\label{sec:examplecomplex}

%include ../iohk-monitoring/examples/complex/Main.lhs

\section{Code listings - contra-tracer package}

%include ../contra-tracer/src/Control/Tracer.lhs


\section{Code listings - iohk-monitoring package}


%include ../iohk-monitoring/src/Cardano/BM/Observer/STM.lhs
%include ../iohk-monitoring/src/Cardano/BM/Observer/Monadic.lhs

%include ../iohk-monitoring/src/Cardano/BM/Trace.lhs

%include ../iohk-monitoring/src/Cardano/BM/Setup.lhs

%include ../iohk-monitoring/src/Cardano/BM/Counters.lhs
%include ../iohk-monitoring/src/Cardano/BM/Counters/Common.lhs
%include ../iohk-monitoring/src/Cardano/BM/Counters/Dummy.lhs
%include ../iohk-monitoring/src/Cardano/BM/Counters/Linux.lhs

%include ../iohk-monitoring/src/Cardano/BM/Data/Aggregated.lhs
%include ../iohk-monitoring/src/Cardano/BM/Data/AggregatedKind.lhs
%include ../iohk-monitoring/src/Cardano/BM/Data/Backend.lhs
%include ../iohk-monitoring/src/Cardano/BM/Data/BackendKind.lhs
%include ../iohk-monitoring/src/Cardano/BM/Data/Configuration.lhs
%include ../iohk-monitoring/src/Cardano/BM/Data/Counter.lhs
%include ../iohk-monitoring/src/Cardano/BM/Data/LogItem.lhs
%include ../iohk-monitoring/src/Cardano/BM/Data/Observable.lhs
%include ../iohk-monitoring/src/Cardano/BM/Data/Output.lhs
%include ../iohk-monitoring/src/Cardano/BM/Data/Rotation.lhs
%include ../iohk-monitoring/src/Cardano/BM/Data/Severity.lhs
%include ../iohk-monitoring/src/Cardano/BM/Data/SubTrace.lhs
%include ../iohk-monitoring/src/Cardano/BM/Data/Trace.lhs
%include ../iohk-monitoring/src/Cardano/BM/Data/Tracer.lhs

%include ../iohk-monitoring/src/Cardano/BM/Configuration.lhs
%include ../iohk-monitoring/src/Cardano/BM/Configuration/Model.lhs
%include ../iohk-monitoring/src/Cardano/BM/Configuration/Static.lhs

%include ../iohk-monitoring/src/Cardano/BM/Output/Switchboard.lhs
%include ../iohk-monitoring/src/Cardano/BM/Output/Log.lhs
%include ../iohk-monitoring/src/Cardano/BM/Output/LogBuffer.lhs
%include ../iohk-monitoring/src/Cardano/BM/Output/EKGView.lhs
%include ../iohk-monitoring/src/Cardano/BM/Output/Editor.lhs
%include ../iohk-monitoring/src/Cardano/BM/Output/Aggregation.lhs
%include ../iohk-monitoring/src/Cardano/BM/Output/Monitoring.lhs

\chapter{Testing}

%include ../iohk-monitoring/test/Test.lhs

\printindex

\end{document}
