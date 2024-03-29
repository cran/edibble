% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/menu.R
\name{menu_youden}
\alias{menu_youden}
\title{Youden square design}
\usage{
menu_youden(
  nc = random_integer_small(),
  t = random_integer_small(min = nc + 1),
  seed = random_seed_number()
)
}
\arguments{
\item{nc}{The number of columns.}

\item{t}{The number of treatments.}

\item{seed}{A scalar value for computational reproducibility.}
}
\value{
A recipe Youden square design.
}
\description{
Youden square design
}
\examples{
menu_youden(nc = 4, t = 5)
}
\seealso{
Other recipe-designs: 
\code{\link{menu_bibd}()},
\code{\link{menu_crd}()},
\code{\link{menu_factorial}()},
\code{\link{menu_graeco}()},
\code{\link{menu_hyper_graeco}()},
\code{\link{menu_lsd}()},
\code{\link{menu_rcbd}()},
\code{\link{menu_split}()},
\code{\link{menu_strip}()}
}
\concept{recipe-designs}
