% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rcrds.R
\name{expect-vars}
\alias{expect-vars}
\alias{to_be_numeric}
\alias{to_be_integer}
\alias{to_be_date}
\alias{to_be_time}
\alias{to_be_character}
\alias{to_be_factor}
\title{Expected type of data entry}
\usage{
to_be_numeric(range)

to_be_integer(range)

to_be_date(range)

to_be_time(range)

to_be_character(length)

to_be_factor(levels)
}
\arguments{
\item{range, length}{A named list with two elements: "operator" and "value" as
provided by helper \code{with_value()} that gives the possible range of values
that the expected type can take.}

\item{levels}{A character vector with the factor levels.}
}
\value{
A record type.
}
\description{
These functions should be used within \code{expect_vars} where variables that
are to be recorded are constraint to the expected values when exported
as an xlsx file by \verb{export_design().} The functions to set a particular
value type (numeric, integer, date, time and character) are preceded by
"to_be_" where the corresponding restriction set by \code{with_value()}.
}
