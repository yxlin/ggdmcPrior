#include "RcppArmadillo.h"
#include <ggdmcHeaders/prior_type_casting.h>

//' Print a joint distribution
//'
//' The function prints the distribution specification that C++
//' funcitons have received.
//'
//' @name print_prior
//' @param p_prior_r A List containing prior distribution specifications.
//'
//' @return
//' \describe{Character vector describing the prior distribution}
//'
//' @details
//' This function is primarily, for debugging purpose. The prior
//' specification (`p_prior_r`) should be a List with elements from
//' the \code{BuildPrior}.
//'
//' @examples
//' p0 <- c(A = 0.15, B = 0.45, mean_v = 2.25, sd_v = 0.15, t0 = 0.2)
//' p1 <- rep(0.1, 5)
//' p_prior <- BuildPrior(
//'     p0 = p0,
//'     p1 = p1,
//'     lower = rep(NA, 5),
//'     upper = rep(NA, 5),
//'     dist = rep("tnorm", 5),
//'     log_p = rep(TRUE, 5)
//' )
//' pnames <- print_prior(p_prior)
//'
//' @export
// [[Rcpp::export]]
std::vector<std::string> print_prior(const Rcpp::List &p_prior_r)
{
    auto p_prior = new_prior(p_prior_r);
    std::vector<std::string> parameter_names = p_prior_r.names();

    p_prior->print(parameter_names);
    return parameter_names;
}

//' Density and random number generation
//'
//' \code{dprior} computes the density for a joint distribution. \code{rprior}
//' generates random numbers based on the specification in the \code{p_prior_r}
//'
//' @param p_prior_r A List containing distribution specifications (usually
//' specified in \code{BuildPrior} function).
//' @param parameters_r For `dprior`: A NumericVector of parameter values at
//'        which to evaluate the prior density.
//' @param n For `rprior`: Number of random samples to generate (default = 1).
//'
//' @return
//' \describe{
//'   \item{`dprior`}{an R vector of prior density values}
//'   \item{`rprior`}{an R matrix of random samples (n rows × nparameter
//'               columns)}
//'  }
//'
//' @details
//' These functions provide the core computational methods for prior
//' distributions:
//'  \itemize{
//'   \item `dprior`: Evaluates the prior probability density function
//'   \item `rprior`: Generates random samples from the prior distribution
//' }
//'
//' @examples
//' p0 <- c(A = 0.15, B = 0.45, mean_v = 2.25, sd_v = 0.15, t0 = 0.2)
//' p1 <- rep(0.1, 5)
//' p_prior <- BuildPrior(
//'     p0 = p0,
//'     p1 = p1,
//'     lower = rep(NA, 5),
//'     upper = rep(NA, 5),
//'     dist = rep("tnorm", 5),
//'     log_p = rep(TRUE, 5)
//' )
//'
//' # Test dprior -----------------
//' parameters <- seq(0.1, 0.5, 0.1)
//' res0 <- dprior(p_prior, parameters)
//' res1 <- dnorm(parameters, p0, 0.1, TRUE)
//' print(res0)
//' print(res1)
//'
//' # Test rprior -----------------
//' res2 <- rprior(p_prior, 1)
//' print(res2)
//' res3 <- rprior(p_prior, 2)
//' print(res3)
//' @export
// [[Rcpp::export]]
std::vector<double> dprior(const Rcpp::List &p_prior_r,
                           const Rcpp::NumericVector &parameters_r)
{
    auto p_prior = new_prior(p_prior_r);
    auto parameters = Rcpp::as<std::vector<double>>(parameters_r);
    std::vector<double> out(parameters.size());
    p_prior->dprior(parameters, out);
    return out;
}

//' @rdname dprior
//' @export
// [[Rcpp::export]]
Rcpp::NumericMatrix rprior(const Rcpp::List &p_prior_r, unsigned int n = 1)

{
    auto p_ptr = new_prior(p_prior_r);
    arma::mat param_matrix(p_ptr->m_nparameter, n);

    for (size_t i = 0; i < n; ++i)
    {
        param_matrix.col(i) = p_ptr->rprior();
    }

    return Rcpp::wrap(param_matrix);
}

//' Sum of joint densities
//'
//' Computes the sum of joint densities for a vector of parameters given
//' the distribution specification. The function computes log densities
//' only when the \code{log_p} is set to TRUE. This function can be
//' used in Bayesian computations where the joint prior is the product
//' of independent priors (sum in log-space).
//'
//' @param p_prior_r A List of prior specifications, where each element is
//'                  itself a List containing the distribution details
//'                  for one parameter. Each sublist should contain:
//'   \itemize{
//'      \item \code{p0}: The first parameter of the distribution.
//'      \item \code{p1}: The second parameter of the distribution.
//'      \item \code{lower}: The lower bound of the distribution.
//'      \item \code{upper}: The upper bound of the distribution.
//'      \item \code{dist}: A numeric code representing the distribution type.
//'      \item \code{log_p}: Logical indicating whether probabilities
//'                          are logged.
//'    }
//' @param parameters_r A NumericVector of parameter values at which to
//'                     evaluate the densities. The length should match
//'                     the length of \code{p_prior_r}.
//'
//' @return A single double value representing the sum of log-prior densities:
//'         \deqn{\sum_{i=1}^{k} \log p(\theta_i | \text{prior}_i)}
//'         where \eqn{k} is the number of parameters.
//'
//' @details
//' This function:
//' \itemize{
//'   \item Iterates through each parameter and its prior specification
//'   \item Computes the log-density for each parameter value
//'   \item Sums all log-density values
//'   \item Returns the total sum (useful for log-posterior computation)
//' }
//'
//'
//' @examples
//' p0 <- c(A = 0.15, B = 0.45, mean_v = 2.25, sd_v = 0.15, t0 = 0.2)
//' tnorm_prior <- BuildPrior(
//'     p0 = p0,
//'     p1 = rep(1, 5),
//'     lower = rep(0, 5),
//'     upper = rep(NA, 5),
//'     dist = rep("tnorm", 5),
//'     log_p = rep(TRUE, 5)
//' )
//'
//' npar <- length(tnorm_prior)
//' result <- sumlogprior(p_prior_r = tnorm_prior,
//'                       parameters_r = runif(npar, 0, 10))
//' @export
// [[Rcpp::export]]
double sumlogprior(const Rcpp::List &p_prior_r,
                   const Rcpp::NumericVector &parameters_r)
{
    unsigned int nparameter0 = p_prior_r.size();
    unsigned int nparameter1 = parameters_r.size();
    if (nparameter0 != nparameter1)
    {
        Rcpp::stop("Parameter length mismatch:\n"
                   "  - p_prior_r contains " +
                   std::to_string(nparameter0) +
                   " elements\n"
                   "  - parameters_r contains " +
                   std::to_string(nparameter1) +
                   " elements\n"
                   "These vectors must have the same length.");
    }

    auto p_prior = new_prior(p_prior_r);
    arma::vec parameters = Rcpp::as<arma::vec>(parameters_r);
    return p_prior->sumlogprior(parameters);
}

/* -- The truncated normal distributions (external)-- */
//' Truncated normal distribution
//'
//' Density, distribution function, and random generation for the truncated
//' normal distribution with mean `p0`, standard deviation `p1`, and bounds
//' [`lower`, `upper`].
//'
//' @name tnorm
//' @aliases rtnorm ptnorm dtnorm
//' @param n Number of random variates to generate (for `rtnorm`).
//' @param x Vector of quantiles (for `dtnorm` and `ptnorm`).
//' @param p0 Mean of the underlying normal distribution.
//' @param p1 Standard deviation of the underlying normal distribution.
//' @param lower Lower truncation bound.
//' @param upper Upper truncation bound.
//' @param lower_tail Logical; if TRUE (default), probabilities are
//'         \deqn{P[X \leq x]}, otherwise P[X > x].
//' @param log_p Logical; if TRUE, probabilities/densities p are returned as
//' log(p).
//'
//' @return
//' \describe{
//'   \item{`rtnorm`}{NumericVector of random variates from the truncated
//'          normal}
//'  \item{`ptnorm`}{NumericVector of cumulative probabilities}
//'  \item{`dtnorm`}{NumericVector of density values}
//' }
//'
//'  @details
//'  These functions implement the truncated normal distribution:
//'  \itemize{
//'    \item `rtnorm`: Random number generation using inverse transform sampling
//'    \item `ptnorm`: Cumulative distribution function (CDF)
//'    \item `dtnorm`: Probability density function (PDF)
//'  }
//'
//' The distribution is defined as:
//' \deqn{X \sim \mathcal{N}(\mu, \sigma^2), \text{truncated to } [a, b]}
//' where \eqn{\mu = p0}, \eqn{\sigma = p1}, \eqn{a = lower}, \eqn{b = upper}.
//'
//' @references
//' \itemize{
//'   \item RcppTN package by Jonathan Olmsted (RcppTN 0.1-8)
//'         \url{https://github.com/olmjo/RcppTN}
//'   \item msm package by Christopher Jackson
//'         \url{https://cran.r-project.org/package=msm}
//'   \item Robert, C.P. (1995). "Simulation of truncated normal variables".
//'     Statistics and Computing, 5(2), 121-125. \doi{10.1007/BF00143942}
//'  }
//'
//' @examples
//' n <- 1e5
//' p0 <- 0
//' p1 <- 1
//' lower <- 0
//' upper <- Inf
//' rtnorm_dat <- rtnorm(n, p0, p1, lower, upper)
//'
//' ## dtnorm example
//' x <- seq(-5, 5, length.out = 1e3)
//' dtnorm_dat <- dtnorm(x, p0, p1, lower = -2, upper = 2, log_p = FALSE)
//' q <- seq(-5, 5, length.out = 1e3)
//'
//' ptnorm_dat <- ptnorm(q, p0, p1, lower = -2, upper = 3, lower_tail = TRUE,
//'                  log_p = FALSE)
//'
//' cex_lab <- 1
//' cex_axis <- 0.5
//' line_width <- 1.5
//'
//' hist(rtnorm_dat, breaks = "fd", freq = FALSE, xlab = "", cex.lab = cex_lab,
//' cex.axis = cex_axis, main = "" )
//' plot(x, dtnorm_dat, type = "l", lwd = line_width, xlab = "",
//' ylab = "Density", cex.lab = cex_lab, cex.axis = cex_axis, main = "")
//'
//' @export
// [[Rcpp::export]]
std::vector<double> rtnorm(unsigned int n, double p0, double p1, double lower,
                           double upper)
{
    std::vector<double> out(n);
    tnorm::tnorm_class tnorm_instance(p0, p1, lower, upper);
    std::generate(out.begin(), out.end(),
                  [&tnorm_instance]() { return tnorm_instance.r(); });
    return out;
}

//' @rdname tnorm
//' @export
// [[Rcpp::export]]
std::vector<double> ptnorm(std::vector<double> x, double p0, double p1,
                           double lower, double upper, bool lower_tail,
                           bool log_p = false)
{
    std::vector<double> out(x.size());
    tnorm::tnorm_class tnorm_instance(p0, p1, lower, upper, lower_tail, log_p);
    tnorm_instance.p(x, out);
    return out;
}

//' @rdname tnorm
//' @export
// [[Rcpp::export]]
std::vector<double> dtnorm(std::vector<double> x, double p0, double p1,
                           double lower, double upper, bool log_p = false)
{
    std::vector<double> out(x.size());
    tnorm::tnorm_class tnorm_instance(p0, p1, lower, upper, log_p);
    tnorm_instance.d(x, out);
    return out;
}
