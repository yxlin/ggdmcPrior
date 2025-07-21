## Prior class -----------------------------------

#' An S4 Class to Represent a Joint Prior Distribution
#'
#' This class encapsulates the structure of prior distributions used in 
#' hierarchical Bayesian modelling. It stores both subject-level and 
#' population-level (hyperparameter) priors for a model’s parameters, 
#' and is used in Bayesian inference workflows, particularly with 
#' models from the \pkg{lbaModel} or \pkg{ddModel} packages.
#'
#' @slot nparameter Integer. Number of free parameters in the model.
#' @slot pnames Character vector. Names of the free parameters.
#' @slot p_prior List. Represents the joint prior distribution at the subject
#' level, usually constructed from standard or truncated distributions.
#' @slot h_prior List. Representing the joint prior at the population 
#' level, typically containing location and scale parameters for hierarchical
#' models. The 'h' prefix refers to hyperparameters.
#'
#' @section Structure:
#' An object of class \code{"prior"} contains the following components:
#' \describe{
#'   \item{\code{nparameter}}{Number of free parameters.}
#'   \item{\code{pnames}}{Names of the model's free parameters.}
#'   \item{\code{p_prior}}{Subject-level prior specification. Conceptually
#' analogous to the model likelihood in a hierarchical Bayesian model.}
#'   \item{\code{h_prior}}{Hyperparameter-level (group-level) prior specification.}
#' }
#'
#' @section Usage:
#' Used to define priors for hierarchical Bayesian cognitive models. This 
#' class allows structured specification of priors at both individual and 
#' group levels. Prior objects are commonly constructed using 
#' \code{\link{set_priors}}, which integrates multiple \code{\link{BuildPrior}} 
#' outputs into a single prior structure.
#'
#' @return An S4 object of class \code{"prior"}, used in computing prior 
#' densities and visualising prior distributions.
#' 
#' @seealso \code{\link{BuildPrior}}
#'
#' @export
setClass("prior",
  slots = c(
    nparameter = "integer", # Number of parameters
    pnames = "character", # Parameter names
    p_prior = "list", # Prior for p
    h_prior = "ANY" # Prior for l and s
  ),
  prototype = list(
    nparameter = 99L,
    pnames = NULL,
    p_prior = NULL,
    h_prior = NULL
  )
)

#' Set up Prior Distributions
#'
#' Configures a set of joint prior distributions for:
#' \itemize{
#'   \item Subject-level parameters (\code{p_prior}), which also serve as the
#' likelihood function in population-level models.
#'   \item Population-level location parameters (\code{l_prior}).
#'   \item Population-level scale parameters (\code{s_prior}).
#' }
#'
#' @param p_prior A list specifying prior distributions for subject-level
#' parameters (or the likelihood function for the population-level model).
#' Each element in the list should contain:
#'   \itemize{
#'     \item \code{p0}: The first parameter of the distribution.
#'     \item \code{p1}: The second parameter of the distribution.
#'     \item \code{lower}: The lower bound of the distribution.
#'     \item \code{upper}: The upper bound of the distribution.
#'     \item \code{dist}: A numeric code representing the distribution type.
#'     \item \code{log_p}: Logical indicating whether to compute in log space.
#'   }
#'
#' @param l_prior Optional list specifying prior distributions for
#' population-level location parameters. Should have the same structure as
#' \code{p_prior}. Defaults to \code{NULL}.
#'
#' @param s_prior Optional list specifying prior distributions for
#' population-level scale parameters. Should have the same structure as
#' \code{p_prior}. Defaults to \code{NULL}.
#'
#' @return An S4 object of class \code{"prior"} with the following slots:
#' \itemize{
#'   \item \code{nparameter}: Integer; number of parameters in the joint prior.
#'   \item \code{pnames}: Character vector of parameter names.
#'   \item \code{p_prior}: List containing prior specifications for
#' subject-level parameters.
#'   \item \code{h_prior}: List containing merged prior specifications for
#' \code{l_prior} and \code{s_prior}.
#' }
#'
#' @details
#' This function performs the following:
#' \itemize{
#'   \item Validates the structure of all prior specifications.
#'   \item Ensures required distribution parameters are present and bounds are
#'  valid.
#'   \item Merges \code{l_prior} and \code{s_prior} into a single \code{h_prior}
#'  using \code{.merge_prior}.
#'   \item Returns a structured \code{prior} object for use in model fitting
#' and simulation.
#' }
#'
#' The argument \code{log_p} should be set to \code{TRUE} for density
#' evaluation and \code{FALSE} when generating samples (e.g., for initial
#' parameter values).
#'
#' @examples
#' if (requireNamespace("ggdmcModel", quietly = TRUE)) {
#'   BuildModel <- getFromNamespace("BuildModel", "ggdmcModel")
#'
#'   model <- BuildModel(
#'     p_map = list(
#'       A = "1", B = "1", t0 = "1", mean_v = "M", sd_v = "M",
#'       st0 = "1"
#'     ),
#'     match_map = list(M = list(s1 = "r1", s2 = "r2")),
#'     factors = list(S = c("s1", "s2")),
#'     constants = c(sd_v.false = 1, st0 = 0),
#'     accumulators = c("r1", "r2"),
#'     type = "lba"
#'   )
#'
#'   ####################################
#'   # priors for subject-level modelling
#'   ####################################
#'   p0 <- rep(0, model@npar)
#'   names(p0) <- model@pnames
#'   p_prior <- BuildPrior(
#'     p0 = p0,
#'     p1 = rep(10, model@npar),
#'     lower = rep(0, model@npar),
#'     upper = rep(NA, model@npar),
#'     dist = rep("unif", model@npar),
#'     log_p = rep(TRUE, model@npar)
#'   )
#'   sub_priors <- set_priors(p_prior = p_prior)
#'
#'   ####################################
#'   # priors for hierarchical modelling
#'   ####################################
#'   p0 <- runif(model@npar)
#'   names(p0) <- model@pnames
#'   model_likelihood <- BuildPrior(
#'     p0 = p0,
#'     p1 = rep(10, model@npar),
#'     lower = rep(0, model@npar),
#'     upper = rep(NA, model@npar),
#'     dist = rep("tnorm", model@npar),
#'     log_p = rep(TRUE, model@npar)
#'   )
#'
#'   p0 <- rep(0, model@npar)
#'   names(p0) <- model@pnames
#'   l_prior <- BuildPrior(
#'     p0 = p0,
#'     p1 = rep(10, model@npar),
#'     lower = rep(0, model@npar),
#'     upper = rep(NA, model@npar),
#'     dist = rep("unif", model@npar),
#'     log_p = rep(TRUE, model@npar)
#'   )
#'   s_prior <- BuildPrior(
#'     p0 = p0,
#'     p1 = rep(10, model@npar),
#'     lower = rep(NA, model@npar),
#'     upper = rep(NA, model@npar),
#'     dist = rep("unif", model@npar),
#'     log_p = rep(TRUE, model@npar)
#'   )
#'
#'   pop_priors <- set_priors(
#'     p_prior = model_likelihood,
#'     l_prior = l_prior, s_prior = s_prior
#'   )
#' }
#' @importFrom methods new
#' @export
set_priors <- function(p_prior, l_prior = NULL, s_prior = NULL) {
  if (!is.null(l_prior)) {
    if (length(p_prior) != length(l_prior)) {
      stop("p_prior and l_prior must have the same number of parameters")
    }
  }

  if (!is.null(s_prior)) {
    if (length(p_prior) != length(s_prior)) {
      stop("p_prior and s_prior must have the same number of parameters")
    }
  }


  if (is.null(l_prior) && is.null(s_prior)) {
    nparameter <- length(p_prior)
    pnames <- names(p_prior)
    out <- new("prior",
      nparameter = as.integer(nparameter),
      pnames = pnames,
      p_prior = p_prior,
      h_prior = NULL
    )
  } else {
    h_prior <- .merge_priors(l_prior, s_prior)
    nparameter <- length(h_prior)
    pnames <- names(h_prior)

    out <- new("prior",
      nparameter = as.integer(nparameter),
      pnames = pnames,
      p_prior = p_prior,
      h_prior = h_prior
    )
  }

  return(out)
}
