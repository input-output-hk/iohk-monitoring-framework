\documentclass[11pt,twoside]{report}
\pagestyle{headings}

\usepackage{kpfonts}
\usepackage[margin=1in]{geometry}
\usepackage[pdfpagelabels]{hyperref}
\usepackage{todonotes}
\usepackage{amsmath}
\usepackage{mathtools}
\usepackage{verbatim}
\usepackage{colortbl}
\usepackage{hyperref}

%include polycode.fmt

%include ../src/local.fmt
%include references.fmt

\title{Testing benchmarking and logging}
\author{Alexander Diemand
  \and
        Andreas Triantafyllos}
\date{November 2018}

\begin{document}

\hypersetup{pageanchor=false}
\begin{titlepage}
\maketitle
\end{titlepage}

\hypersetup{pageanchor=true}
\pagenumbering{arabic}

\tableofcontents

\begin{abstract}
abstract ...
\end{abstract}

\chapter{Test coverage}

\section{Coverage}

Test coverage is calculated as the fraction of functions which are called from test routines.
This percentage is calculated by the tool |hpc| with a call to 
\begin{verbatim}cabal new-test\end{verbatim}
Add to a local \tt{cabal.project.local} file these lines:
\newline
\begin{tabular}{l r}
tests: & True \\
coverage: & True \\
library-coverage: & True \\
\end{tabular}

\begin{figure}[htp]
\centering{
  \input{test-coverage.tex}
}
\caption{Test coverage of modules in percent as computed by the tool 'hpc'}\label{fig:coverage}
\end{figure}



\chapter{Testing}

\section{Test main entry point}

\begin{code}
module Main
  (
    main
  ) where

import           Test.Tasty

import qualified Cardano.BM.Test.Aggregated (tests)
import qualified Cardano.BM.Test.STM (tests)
import qualified Cardano.BM.Test.Trace (tests)
import qualified Cardano.BM.Test.Configuration (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "iohk-monitoring"
  [ Cardano.BM.Test.Aggregated.tests
  , Cardano.BM.Test.STM.tests
  , Cardano.BM.Test.Trace.tests
  , Cardano.BM.Test.Configuration.tests
  ]
\end{code}

%include Cardano/BM/Arbitrary/Aggregated.lhs

%include Cardano/BM/Test/Aggregated.lhs
%include Cardano/BM/Test/STM.lhs
%include Cardano/BM/Test/Trace.lhs
%include Cardano/BM/Test/Configuration.lhs

\end{document}
