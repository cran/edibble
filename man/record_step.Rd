% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{record_step}
\alias{record_step}
\title{Record the coding step}
\usage{
record_step()
}
\value{
Returns nothing.
}
\description{
Call this function in functions that modify the edibble design or table so
the step is tracked. The output of functions using \code{record_step()} should
be returning an edibble design or table.
}
