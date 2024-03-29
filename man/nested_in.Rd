% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nest.R
\name{nested_in}
\alias{nested_in}
\alias{conditioned_on}
\title{Specify the nesting or conditional structure for units or treatments}
\usage{
nested_in(x, ...)

conditioned_on(x, ...)
}
\arguments{
\item{x}{The name of the parent unit to nest under.}

\item{...}{a single number OR a sequence of two-sided formula where the
left-hand side corresponds to the name of the level (or the level number) of \code{x}
and the right-hand side is an integer specifying the number of levels nested under the
corresponding levels.}
}
\value{
A nested level.
}
\description{
Conditional treatment is different to nested units as the levels are assumed to be
distinct for the latter but not for the former.
}
\details{
Currently when specifying conditional treatment, only character vectors
are accepted on the RHS.
}
\examples{
design("Split-Plot Design | Split-Unit Design") \%>\%
  set_units(mainplot = 60,
            subplot = nested_in(mainplot, 10))
}
\seealso{
See \code{\link[=set_units]{set_units()}} for examples of how to use this.
}
