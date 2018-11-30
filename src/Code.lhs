\documentclass[11pt,twoside]{report}
\pagestyle{headings}

\usepackage{kpfonts}
\usepackage[margin=1in]{geometry}
\usepackage[pdfpagelabels]{hyperref}
\usepackage{todonotes}
\usepackage{amsmath}
\usepackage{mathtools}
\usepackage{colortbl}
\usepackage{hyperref}
\usepackage{verbatim}
\usepackage{graphicx}
\usepackage{pstricks}

%include colorcode.fmt

%include local.fmt

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
abstract ...
\end{abstract}

\newpage

\hypersetup{pageanchor=true}
\pagenumbering{arabic}

\tableofcontents

\newpage

\chapter{Cardano BM}

\section{Introduction}

introduction ...

\section{Overview}

In figure \ref{fig:overview} we display the relationships among modules
in |Cardano.BM|. The arrows indicate import of a module. The relationship with
a triangle at one end would signify "inheritance", but we use it to show
that one module replaces the other in the namespace, thus refines its interface.

\begin{figure}[htp]
\centering{
  \includegraphics[scale=0.54]{../doc/Relationships.pdf}
%%  \def\svgwidth{\columnwidth}
%%  \input{../doc/Relationships.pdf_tex}
%%  \includegraphics{../doc/Relationships.eps}
}
\caption{Overview of module relationships}\label{fig:overview}
\end{figure}

\section{Examples}

examples ...

\section{Code listings}

%if False
\begin{code}
module Code
where

-- import qualified Cardano.BM.Aggregated
-- import qualified Cardano.BM.BaseTrace
-- import qualified Cardano.BM.Controller
-- import qualified Cardano.BM.Counters
-- import qualified Cardano.BM.Data
-- import qualified Cardano.BM.Observer.Monadic
-- import qualified Cardano.BM.Observer.STM
-- import qualified Cardano.BM.Trace

\end{code}
%endif

%include Cardano/BM/Observer/STM.lhs
%include Cardano/BM/Observer/Monadic.lhs

%include Cardano/BM/BaseTrace.lhs

%include Cardano/BM/Trace.lhs

%include Cardano/BM/Controller.lhs

%include Cardano/BM/Counters.lhs
%include Cardano/BM/Counters/Common.lhs
%include Cardano/BM/Counters/Dummy.lhs
%include Cardano/BM/Counters/Linux.lhs

%include Cardano/BM/Data.lhs

%include Cardano/BM/Configuration.lhs
%include Cardano/BM/Configuration/Model.lhs

%include Cardano/BM/Aggregated.lhs

%include Cardano/BM/Output/Switchboard.lhs
%include Cardano/BM/Output/Katip.lhs
%include Cardano/BM/Output/Aggregation.lhs

\end{document}
