% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pivot.R
\name{pivot_trts_widelist}
\alias{pivot_trts_widelist}
\alias{pivot_trts_widetable}
\title{Pivot treatments to a wider list or table format}
\usage{
pivot_trts_widelist(.data, trts = NULL, fcts = NULL, drop = FALSE)

pivot_trts_widetable(.data, trts = NULL, fcts = NULL)
}
\arguments{
\item{.data}{An edibble table.}

\item{trts}{A vector of treatment (tidyselect compatible). By default it is
NULL and includes all the treatments.}

\item{fcts}{A vector of factors in the edibble table.}

\item{drop}{Whether the resulting list should drop to a vector within each list
element if there is only one column. Default is FALSE.}
}
\value{
A named list where elements are the data and the names are treatments.
}
\description{
Pivot treatments to a wider list or table format
}
\examples{
pivot_trts_widelist(takeout(menu_crd(t = 5, n = 20)))
}
\keyword{internal}
