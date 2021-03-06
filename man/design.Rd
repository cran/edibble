% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/design.R
\name{design}
\alias{design}
\alias{redesign}
\title{Start the edibble design}
\usage{
design(name = NULL, .record = TRUE, seed = NULL, kitchen = Kitchen)

redesign(
  .data,
  name = NULL,
  .record = TRUE,
  seed = NULL,
  kitchen = Kitchen,
  ...
)
}
\arguments{
\item{name}{Optional name used as title for printing the design.}

\item{.record}{A logical value. This indicates whether to record this
code step. The default is TRUE. It should remain TRUE unless this
function is used as a wrapper in other code.}

\item{seed}{A seed number for reproducibility.}

\item{kitchen}{An environment setup in a manner to manipulate, extract and query
information on the design.}

\item{.data}{An edibble table.}

\item{...}{Either a name-value pair or a series of the names.}
}
\value{
An empty \code{edbl_design} object.
}
\description{
This function doesn't really do much besides create a new edibble design object.
}
\examples{
design("My design")
}
\seealso{
Add variables to this design with \code{\link[=set_units]{set_units()}}, \code{\link[=set_trts]{set_trts()}}, and
\code{\link[=set_rcrds]{set_rcrds()}}.

Other user-facing functions: 
\code{\link{allot}},
\code{\link{expect_rcrds}()},
\code{\link{export_design}()},
\code{\link{serve_table}()},
\code{\link{set_rcrds}()},
\code{\link{set_trts}()},
\code{\link{set_units}()}
}
\concept{user-facing functions}
