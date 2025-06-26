### Prior class -----------------------------------
#' An S4 class  to represent the joint distribution.
#'
#' @slot nparameter the number of free parameters
#' @slot pnames the names of free parameters
#' @slot p_prior a list storing the subject-level prior (joint) distribution.
#' @slot h_prior a list storing the population-level prior (joint) distribution
#' (location and scale distributions). `h` stands for hyper-level.
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

#' Set up prior distributions
#'
#' Configures a set of joint distributions for (1) the subject-level prior
#' distribution (p_prior) or the population-level model likelihood, (2)
#' the population-level, location parameters (l_prior), and (3) the
#' population-level, scale parameters (s_prior).
#'
#' @param p_prior A list specifying prior distributions for the
#' subject-level (or model likleihood function for the population level)
#' model parameters. Each element should be a list containing:
#'        \itemize{
#'           \item \code{p0}: The first parameter of the distribution.
#'           \item \code{p1}: The second parameter of the distribution.
#'           \item \code{lower}: The lower bound of the distribution.
#'           \item \code{upper}: The upper bound of the distribution.
#'           \item \code{dist}: A numeric code representing the distribution
#'                              type.
#'           \item \code{log_p}: Logical indicating whether probabilities
#'                               are logged.
#' }
#' @param l_prior Optional list specifying priors for location parameters, with the
#'        same structure as `p_prior`. Defaults to NULL.
#' @param s_prior Optional list specifying priors for scale parameters, with the
#'        same structure as `p_prior`. Defaults to NULL.
#'
#' @return A list of class "model_priors" containing three components:
#'         \itemize{
#'           \item \code{p_prior}: Prior specifications for main parameters
#'           \item \code{l_prior}: Prior specifications for location parameters (if provided)
#'           \item \code{s_prior}: Prior specifications for scale parameters (if provided)
#'         }
#'
#' @details
#' This function:
#' \itemize{
#'   \item Validates the structure of all prior specifications
#'   \item Ensures all required distribution parameters are provided
#'         (optionally) checks parameter bounds for validity
#'   \item Returns an organised list that can be used by other model-fitting
#'         functions
#' }
#' `log_p` is usually set to TRUE, for density calculation and set to FALSE for
#' sampling starting theta values. l_prior and s_prior are merged to from one h_prior via
#' the internal \code{.merge_prior} function.
#'
#' @examples
#' \dontrun{
#'
#' model <- ggdmcModel::BuildModel(
#'   p_map = list(
#'     A = "1", B = "1", t0 = "1", mean_v = "M", sd_v = "M",
#'     st0 = "1"
#'   ),
#'   match_map = list(M = list(s1 = "r1", s2 = "r2")),
#'   factors = list(S = c("s1", "s2")),
#'   constants = c(sd_v.false = 1, st0 = 0),
#'   accumulators = c("r1", "r2"),
#'   type = "lba"
#' )
#' p0 <- rep(0, model@npar)
#' names(p0) <- model@pnames
#' p_prior <- ggdmcPrior::BuildPrior(
#'   p0 = p0,
#'   p1 = rep(10, model@npar),
#'   lower = rep(0, model@npar),
#'   upper = rep(NA, model@npar),
#'   dist = rep("unif", model@npar),
#'   log_p = rep(TRUE, model@npar)
#' )
#' sub_priors <- set_priors(p_prior = p_prior)
#'
#' # priors for hierarchical modelling
#' p0 <- runif(model@npar)
#' names(p0) <- model@pnames
#' model_likelihood <- ggdmcPrior::BuildPrior(
#'   p0 = p0,
#'   p1 = rep(10, model@npar),
#'   lower = rep(0, model@npar),
#'   upper = rep(NA, model@npar),
#'   dist = rep("tnorm", model@npar),
#'   log_p = rep(TRUE, model@npar)
#' )
#'
#' p0 <- rep(0, model@npar)
#' names(p0) <- model@pnames
#' l_prior <- ggdmcPrior::BuildPrior(
#'   p0 = p0,
#'   p1 = rep(10, model@npar),
#'   lower = rep(0, model@npar),
#'   upper = rep(NA, model@npar),
#'   dist = rep("unif", model@npar),
#'   log_p = rep(TRUE, model@npar)
#' )
#' s_prior <- ggdmcPrior::BuildPrior(
#'   p0 = p0,
#'   p1 = rep(10, model@npar),
#'   lower = rep(NA, model@npar),
#'   upper = rep(NA, model@npar),
#'   dist = rep("unif", model@npar),
#'   log_p = rep(TRUE, model@npar)
#' )
#'
#' pop_priors <- ggdmcPrior::set_priors(
#'   p_prior = model_likelihood,
#'   l_prior = l_prior, s_prior = s_prior
#' )
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
