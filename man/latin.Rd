% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/latin.R
\name{latin}
\alias{latin}
\alias{latin_square}
\alias{latin_rectangle}
\alias{latin_array}
\title{Latin square designs and its generalisations as an array}
\usage{
latin_square(n, randomise = TRUE)

latin_rectangle(nr, nc, nt, randomise = TRUE)

latin_array(dim, nt, randomise = TRUE)
}
\arguments{
\item{n, nt}{The number of treatments}

\item{randomise}{A logical value to indicate whether the treatment allocation should be randomised. The default value is \code{TRUE}.}

\item{nr}{The number of rows}

\item{nc}{The number of columns}

\item{dim}{A vector of integers to indicate the number of elements in each dimension.}
}
\description{
Latin square designs and its generalisations as an array
}
\section{Functions}{
\itemize{
\item \code{latin_square()}: Latin square design

\item \code{latin_rectangle()}: Like a Latin square design but allow different number of rows and columns

\item \code{latin_array()}: Returns an array where it stitches up multiple Latin square/rectangle design

}}
\examples{
latin_square(n = 3)
latin_rectangle(3, 3, 3)
latin_array(c(3, 3, 3), 3)
}
