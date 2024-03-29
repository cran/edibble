% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/menu.R
\name{menu_hyper_graeco}
\alias{menu_hyper_graeco}
\title{Hyper-Graeco-Latin Square Design}
\usage{
menu_hyper_graeco(t = random_integer_small(), seed = random_seed_number())
}
\arguments{
\item{t}{The number of treatments}

\item{seed}{A scalar value for computational reproducibility.}
}
\value{
A recipe Hyper-Graeco-Latin square design.
}
\description{
Hyper-Graeco-Latin Square Design
}
\examples{
menu_hyper_graeco(t = 3)
}
\seealso{
Other recipe-designs: 
\code{\link{menu_bibd}()},
\code{\link{menu_crd}()},
\code{\link{menu_factorial}()},
\code{\link{menu_graeco}()},
\code{\link{menu_lsd}()},
\code{\link{menu_rcbd}()},
\code{\link{menu_split}()},
\code{\link{menu_strip}()},
\code{\link{menu_youden}()}
}
\concept{recipe-designs}
