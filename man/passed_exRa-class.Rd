% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/passed_exRa.R
\docType{class}
\name{passed_exRa-class}
\alias{passed_exRa-class}
\title{class passed_exRa, an S4 class representing
 an a result of the exhaustive_tests function}
\description{
class passed_exRa, an S4 class representing
 an a result of the exhaustive_tests function
}
\section{Slots}{

\describe{
\item{\code{process}}{a data.frame containg process information
from the call to exhaustive_tests}

\item{\code{passed_combos}}{a list of vectors containing item combinations
using the indices of the items}

\item{\code{passed_models}}{a list of objects of the class the respective package
used for estimation uses for its models}

\item{\code{passed_p.par}}{a list of objects containing the person parameters.
Depending on the package used for estimation, also residuals and/or
the PSI value can be part of this list}

\item{\code{IC}}{a data.frame containing information criteria for each of the
models in passed_models}

\item{\code{data}}{a data.frame containing the data used for the analyses.}

\item{\code{timings}}{a data.frame containing the the timings of the analyses.}
}}

