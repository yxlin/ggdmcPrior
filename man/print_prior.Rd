% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{print_prior}
\alias{print_prior}
\title{Print a joint distribution}
\usage{
print_prior(p_prior_r)
}
\arguments{
\item{p_prior_r}{A List containing prior distribution specifications.}
}
\value{
\describe{Character vector describing the prior distribution}
}
\description{
The function prints the distribution specification that C++
funcitons have received.
}
\details{
This function is primarily, for debugging purpose. The prior
specification (`p_prior_r`) should be a List with elements from
the \code{BuildPrior}.
}
\examples{
p0 <- c(A = 0.15, B = 0.45, mean_v = 2.25, sd_v = 0.15, t0 = 0.2)
p1 <- rep(0.1, 5)
p_prior <- BuildPrior(
    p0 = p0,
    p1 = p1,
    lower = rep(NA, 5),
    upper = rep(NA, 5),
    dist = rep("tnorm", 5),
    log_p = rep(TRUE, 5)
)
pnames <- print_prior(p_prior)

}
