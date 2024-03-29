% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/menu.R
\name{menu_lsd}
\alias{menu_lsd}
\title{Prepare classical Latin square design}
\usage{
menu_lsd(t = random_integer_small(), seed = random_seed_number())
}
\arguments{
\item{t}{The number of treatments}

\item{seed}{A scalar value for computational reproducibility.}
}
\value{
A recipe Latin square design.
}
\description{
Prepare classical Latin square design
}
\examples{
menu_lsd(t = 3)
}
\seealso{
Other recipe-designs: 
\code{\link{menu_bibd}()},
\code{\link{menu_crd}()},
\code{\link{menu_factorial}()},
\code{\link{menu_graeco}()},
\code{\link{menu_hyper_graeco}()},
\code{\link{menu_rcbd}()},
\code{\link{menu_split}()},
\code{\link{menu_strip}()},
\code{\link{menu_youden}()}
}
\concept{recipe-designs}
