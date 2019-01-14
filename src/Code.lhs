\documentclass[11pt,twoside]{report}

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
\usepackage{makeidx}
\makeindex

%include colorcode.fmt

%include local.fmt
%include references.fmt

\title{Cardano.BM - benchmarking and logging}
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

\chapter{Cardano BM}

\section{Overview}

In figure \ref{fig:overview} we display the relationships among modules
in |Cardano.BM|. The arrows indicate import of a module. The arrows with
a triangle at one end would signify "inheritance" in object-oriented programming,
but we use it to show that one module replaces the other in the namespace,
thus refines its interface.

\begin{figure}[h]
\centering{
  \includegraphics[scale=0.48]{OverviewModules.pdf}
}
\caption{Overview of module relationships}\label{fig:overview}
\end{figure}

\section{Introduction}

\subsection{Logging with |Trace|}

\subsection{Setup procedure}

\begin{figure}[htp]
\centering{
  \includegraphics[scale=0.54]{SetupProcedure.pdf}
}
\caption{Setup procedure}\label{fig:setup}
\end{figure}

\subsubsection{Hierarchy of |Trace|s}

\subsection{Measuring |Observable|s}
\subsection{Information reduction in |Aggregation|}
\subsection{Output selection}
\subsection{Monitoring}

\section{Examples}

\subsection{Observing evaluation of a STM action}

\subsection{Observing evaluation of a monad action}

\subsection{Simple example showing plain logging}

%include ../examples/simple/Main.lhs

\subsection{Complex example showing logging, aggregation of log items, and observing |IO| actions}

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
%include Cardano/BM/Data/Backend.lhs
%include Cardano/BM/Data/Configuration.lhs
%include Cardano/BM/Data/Counter.lhs
%include Cardano/BM/Data/LogItem.lhs
%include Cardano/BM/Data/Observable.lhs
%include Cardano/BM/Data/Output.lhs
%include Cardano/BM/Data/Severity.lhs
%include Cardano/BM/Data/SubTrace.lhs
%include Cardano/BM/Data/Trace.lhs

%include Cardano/BM/Configuration.lhs
%include Cardano/BM/Configuration/Model.lhs

%include Cardano/BM/Output/Switchboard.lhs
%include Cardano/BM/Output/Log.lhs
%include Cardano/BM/Output/EKGView.lhs
%include Cardano/BM/Output/Aggregation.lhs

\printindex

\end{document}
