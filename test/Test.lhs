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

%include polycode.fmt

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

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "iohk-monitoring"
  [ Cardano.BM.Test.Aggregated.tests
  , Cardano.BM.Test.STM.tests
  , Cardano.BM.Test.Trace.tests
  ]
\end{code}

%include Cardano/BM/Arbitrary/Aggregated.lhs

%include Cardano/BM/Test/Aggregated.lhs
%include Cardano/BM/Test/STM.lhs
%include Cardano/BM/Test/Trace.lhs

\end{document}
