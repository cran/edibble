% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/menu.R
\name{scan_menu}
\alias{scan_menu}
\title{Find the short names of the named designs}
\usage{
scan_menu(packages = NULL, exclude = NULL)
}
\arguments{
\item{packages}{A character vector containing the package names to search
named designs from. By default it will search edibble and other packages loaded.}

\item{exclude}{A character vector denoting the packages to exclude search from.}
}
\value{
A data.frame with package, name, arguments, and full name.
}
\description{
Find the short names of the named designs
}
\examples{
scan_menu()
}
