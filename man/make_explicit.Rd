% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_explicit.R
\name{make_explicit}
\alias{make_explicit}
\title{Replace implicit function calls with explicit ones}
\usage{
make_explicit(input_path, output_path = input_path, comments = FALSE)
}
\arguments{
\item{input_path}{A file with R code}

\item{output_path}{If different from \code{input_path}}

\item{comments}{Should code be replaced in comments? (Default: FALSE)}
}
\value{
the output_path
}
\description{
Replace implicit function calls with explicit ones
}
