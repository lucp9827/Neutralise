% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Neutralise_vsc.R
\name{Neutralise_vsc}
\alias{Neutralise_vsc}
\title{Neutralise Main function}
\usage{
Neutralise_vsc(
  path,
  Test = NULL,
  Data.Generator = NULL,
  settings = NULL,
  B = NULL,
  N = 1000,
  data.name,
  method.name
)
}
\arguments{
\item{path}{is the directory path of neutralisefiles}

\item{Test}{is a function of a statistical test, if not specified (=NULL default) it will evaluate all statistical methods in the Methods directory if these are not evaluated yet.}

\item{Data.Generator}{is a function of a data generator, if not specified (=NULL default) it will simulate data using all data generators in the Data directory and settings in the Setting directory, if these are not simulated yet.}
}
\value{
results of the analysis (test statistic - p-value - power calculations) in the Results directory
}
\description{
Neutralise Main function
}
\examples{
see Demonstration.Rmd
}
