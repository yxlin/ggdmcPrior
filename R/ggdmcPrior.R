#' ggdmc module for standard and truncated distributions
#'
#' \pkg{ggdmcPrior} provides functions for specifying and evaluating standard
#' distributions, designed to work with the ggdmc package. It supports Bayesian
#' computation and includes utilities for density calculation, sampling,
#' and visualisation.
#'
#' @keywords package
#' @name ggdmcPrior
#' @keywords internal
#' @author  Yi-Shin Lin <yishinlin001@gmail.com>
#' @importFrom Rcpp evalCpp
#' @useDynLib ggdmcPrior
"_PACKAGE"
NULL

### Helper functions ------

#' @importFrom stats setNames
.merge_priors <- function(location, scale, loc_name = "loc_", sca_name = "sca_") {
    loc_named <- setNames(location, paste0(loc_name, names(location)))
    scale_named <- setNames(scale, paste0(sca_name, names(scale)))
    c(loc_named, scale_named)
}

create_param_list <- function(p0, p1, lower, upper, dist_id, log_p) {
    # Handle NA values for lower and upper based on dist_id
    if (dist_id == 1) { # tnorm
        if (is.na(lower)) lower <- -Inf
        if (is.na(upper)) upper <- Inf
    } else if (dist_id == 2) { # beta
        if (is.na(lower)) lower <- 0
        if (is.na(upper)) upper <- 1
    } else if (dist_id == 3) { # gamma
        if (is.na(lower)) lower <- 0
        if (is.na(upper)) upper <- Inf
    } else if (dist_id == 4) { # lnorm
        if (is.na(lower)) lower <- 0
        if (is.na(upper)) upper <- Inf
    } else if (dist_id == 5) { # cauchy
        if (is.na(lower)) lower <- -Inf
        if (is.na(upper)) upper <- Inf
    } else if (dist_id == 6) { # unif
        if (is.na(lower)) lower <- p0
        if (is.na(upper)) upper <- p1
    } else {
        # Default case for unknown distributions
        if (is.na(lower)) lower <- -Inf
        if (is.na(upper)) upper <- Inf
    }

    p <- c(p0, p1, lower, upper, dist_id, log_p)
    names(p) <- c("p0", "p1", "lower", "upper", "dist_id", "log_p")
    return(as.list(p))
}

generate_x_values <- function(dist, p, npoint) {
    switch(dist,
        { # 1: Truncated normal (tnorm)
            lb <- pmax(p$lower, p[[1]] - 3 * p[[2]])
            ub <- pmin(p$upper, p[[1]] + 3 * p[[2]])
            seq(lb, ub, length.out = npoint)
        },
        { # 2: Beta (beta)
            # The Beta distribution is defined on the interval [0, 1].  It's a
            # probability distribution describing the probability of proportions
            # or fractions.  Therefore, it doesn't make sense to evaluate it outside
            # of that range
            lb <- p$lower
            ub <- p$upper
            seq(lb, ub, length.out = npoint)
        },
        { # 3: Gamma (gamma)
            lb <- p$lower
            ub <- p[[1]] * p[[2]] + 3 * sqrt(p[[1]]) * p[[2]]
            seq(lb, ub, length.out = npoint)
        },
        { # 4: Log-normal (lnorm)
            lb <- p$lower
            ub <- exp(p[[1]] + 2 * p[[2]])
            seq(lb, ub, length.out = npoint)
        },
        { # 5: Cauchy (cauchy)
            lb <- pmax(p$lower, p[[1]] - 3 * p[[2]])
            ub <- pmin(p$upper, p[[1]] + 3 * p[[2]])
            seq(lb, ub, length.out = npoint)
        },
        { # 6: Uniform (unif)
            seq(p[[1]], p[[2]], length.out = npoint)
        },
        { # 7: Normal (norm)
            lb <- p[[1]] - 3 * p[[2]]
            ub <- p[[1]] + 3 * p[[2]]
            seq(lb, ub, length.out = npoint)
        },
        seq(-10, 10, length.out = npoint) # Default case
    )
}

generate_prior_data <- function(p_prior, npoint = 100L) {
    # Validate inputs
    if (!is.numeric(npoint) || npoint <= 0) stop("npoint must be a positive integer.")
    # npoint = 100
    nparameter <- length(p_prior)
    pnames <- names(p_prior)

    # Preallocate x matrix to store the generated values
    x <- matrix(NA, nrow = npoint, ncol = nparameter)

    for (i in seq_len(nparameter)) {
        pname <- pnames[i]
        p <- p_prior[[i]]
        dist <- p$dist_id
        x[, i] <- generate_x_values(dist, p, npoint) # Fill in the x values directly
    }

    # Preallocate y matrix to store prior density values
    y <- matrix(NA, nrow = npoint, ncol = nparameter)

    # Use vectorized dprior calls
    for (i in seq_len(npoint)) {
        y[i, ] <- dprior(p_prior, x[i, ]) # Calculate prior densities for each point
    }

    # Create a data frame for all parameters
    d <- data.frame(
        x = as.vector(x), # Flatten the x matrix into a vector
        y = as.vector(y), # Flatten the y matrix into a vector
        gp = rep(pnames, each = npoint) # Repeat parameter names for each point
    )

    return(d)
}

### External functions --------------------

#' Visualise distributions
#'
#' Plots density curves for specified distributions to help visualise
#' their shape and domain.
#'
#' @param p_prior A list of distribution specifications, where each element
#' is a list containing:
#'        \describe{
#'          \item{dist}{Character specifying the distribution ("tnorm",
#'                      "beta", "gamma", "lnorm", "cauchy", "unif", "norm")}
#'          \item{p0}{First parameter of the distribution}
#'          \item{p1}{Second parameter of the distribution (where applicable)}
#'          \item{lower}{Optional vector specifying lower bounds (for
#'             truncated distributions)}
#'          \item{upper}{Optional vector specifying upper bounds (for
#'             truncated distributions)}
#'          \item{log_p}{Logical indicating whether to compute log densities}
#'  }
#' @param font_size Numeric base font size for plot labels (default = 5).
#' @param cex Numeric scaling factor for plot elements (default = 5).
#' @param return_data Logical indicating whether to return the calculated
#' density data instead of plotting (default = FALSE).
#'
#' @return If `return_data = FALSE` (default), returns a lattice object showing
#' the prior densities. If `return_data = TRUE`, returns a data frame:
#'         \itemize{
#'           \item \code{x}: \code{npoint} x values generated based on the heuristic
#'                            set in \code{generate_x_value} internal function.
#'           \item \code{y}: the corresponding density values
#'           \item \code{gp}: Parameter names corresponding to each value
#'         }
#'
#' @details
#' This function:
#' \itemize{
#'   \item Automatically determines appropriate x-axis ranges based on each
#'         prior's properties
#'   \item Handles both bounded and unbounded distributions
#'   \item Supports all distribution types available in the package
#' }
#'
#' For truncated distributions, the plot shows the density within the specified
#' bounds. The function automatically generates appropriate axis limits and
#' labels for each prior.
#'
#' @examples
#' # Define a joint distribution
#' p_prior <- BuildPrior(
#'     p0 = c(A = 0.15, B = 0.45, mean_v = 2.25, sd_v = 0.15, t0 = 0.2),
#'     p1 = rep(0.1, 5),
#'     lower = rep(NA, 5),
#'     upper = rep(NA, 5),
#'     dist = rep("tnorm", 5),
#'     log_p = rep(FALSE, 5)
#' )
#'
#' plot_prior(p_prior)
#'
#' @import lattice
#' @export
plot_prior <- function(p_prior, font_size = 5, cex = 5, return_data = FALSE) {
    d <- generate_prior_data(p_prior)

    p0 <- xyplot(y ~ x | gp,
        data = d,
        type = "l", # Line plot
        xlab = "", # Empty x-axis label
        ylab = "Density", # y-axis label
        layout = c(1, length(unique(d$gp))), # Arrange facets in 1 row
        auto.key = FALSE,
        scales = list(relation = "free", cex = cex),
        par.settings = list(
            fontsize = list(text = font_size, points = 10), # General font size
            par.xlab.text = list(cex = cex), # X-axis label size (if needed)
            par.ylab.text = list(cex = cex) # Y-axis label size (if needed)
        ),
        par.strip.text = list(cex = font_size * 0.9) # Adjust facet label (strip) font size
    )

    if (return_data) {
        return(d)
    } else {
        print(p0)
        invisible(p0)
    }
}


#' Build a joint prior distribution
#'
#' \code{BuildPrior} sets up a joint distribution of the prior. Each model
#' parameter is assigned one probability distribution.
#
#' \code{p0} and \code{p1} refer to the first and second parameters.
#' I use the convention of the 0-based index to work with the C++ and the
#' Python system package (coming soon). \code{p0} must comes with parameter
#' names.
#'
#' Seven distributions are implemented:
#' \enumerate{
#' \item Truncated normal distribution, where: \code{p0 = mean}, \code{p1 = sd}.
#'       When the lower and upper bounds are not provided, they are set
#'       to \code{-Inf} and \code{Inf}, rendering a normal distribution
#'       (see \link{tnorm}). Type name is \code{"tnorm"}.
#' \item Beta distribution, where: \code{p0 = shape1} and \code{p1 = shape2}
#'       (see \link{pbeta}). Note the uniform distribution is a special case
#'       of the beta with \code{p0 = 1} and \code{p1 = 1}. Type name is
#'       \code{"beta"}.
#' \item Gamma distribution, where \code{p0 = shape} and \code{p1 = scale}
#'       (see \link{pgamma}). Note \code{p1} is scale, not rate. Type name is
#'       \code{"gamma"}.
#' \item Log-normal, where \code{p0 = meanlog} and \code{p1 = sdlog}
#'       (see \link{plnorm}). Type name is \code{"lnorm"}.
#' \item Cauchy distribution, where \code{p0 = location} and \code{p1 = scale}
#'       (see \link{pcauchy}). Type name is \code{"cauchy"}.
#' \item Uniform distribution, where \code{p0 = lower} and \code{p1 = upper}
#'       (see \link{punif}). Type name is \code{"unif"}.
#' \item Normal distribution, where \code{p0 = mean} and \code{p1 = sd}
#'       (see \link{pnorm}). Type name is \code{"norm"}.
#' }
#'
#' @param p0 the first parameter of a distribution (e.g., mean, shape1, etc.).
#' @param p1 the second parameter of a distribution (e.g., sd, shape2, etc.).
#' @param lower lower support (boundary). Default is \code{NA},
#' which will be converted to a real value or -Inf based on the distribution
#' type.
#' @param upper upper support (boundary). Default is \code{NA}, which will be
#' converted to a real value or Inf based on the distribution type.
#' @param dists a vector of character strings specifying the distribution type
#' for each parameter. Valid types are: \code{"tnorm"}, \code{"beta"},
#' \code{"gamma"}, \code{"lnorm"}, \code{"cauchy"}, \code{"unif"}, and
#' \code{"norm"}. Default is \code{"norm"}.
#' @param log_p logical; if \code{TRUE}, probabilities are given as
#' \code{log(p)}. Default is \code{TRUE}.
#' @param types available distribution types.
#'
#' @return a list of lists, where each sub-list contains the parameter for its
#' prior definition.
#'         Each sub-list includes:
#'         \itemize{
#'           \item \code{p0}: The first parameter of the distribution.
#'           \item \code{p1}: The second parameter of the distribution.
#'           \item \code{lower}: The lower bound of the distribution.
#'           \item \code{upper}: The upper bound of the distribution.
#'           \item \code{dist}: A numeric code representing the distribution
#'                              type.
#'           \item \code{log_p}: Logical indicating whether probabilities
#'                               are logged.
#'         }
#'
#' @examples
#' # Using dbeta to represent a uniform distribution of bounds(0, 1)
#' x <- seq(-.1, 1.1, .001)
#' plot(x, dbeta(x, 1, 1),
#'     type = "l", ylab = "Density", xlab = "x",
#'     lwd = 2, cex.lab = 1.5, cex.axis = 2
#' )
#'
#' ## Create an S4 prior object
#' p_prior <- BuildPrior(
#'     p0 = c(A = 0.15, B = 0.45, mean_v = 2.25, sd_v = 0.15, t0 = 0.2),
#'     p1 = rep(0.1, 5),
#'     lower = rep(NA, 5),
#'     upper = rep(NA, 5),
#'     dist = rep("tnorm", 5),
#'     log_p = rep(NA, 5)
#' )
#'
#' print_prior(p_prior)
#'
#' # Use the beta distribution to create uniform densities
#' # lower and upper set the bounds. If lower is NA, it will be set to 0.
#' # If upper is NA, it will be set to 1.
#' p_prior <- BuildPrior(
#'     p0 = c(A = 1, B = 1, mean_v = 1, sd_v = 1, t0 = 1),
#'     p1 = rep(1, 5),
#'     lower = rep(0, 5),
#'     upper = rep(5, 5),
#'     dist = rep("beta", 5),
#'     log_p = rep(FALSE, 5)
#' )
#'
#' p0 <- plot_prior(p_prior, font_size = 3.5, cex = 3.5)
#' @export
BuildPrior <- function(p0,
                       p1,
                       lower = rep(NA, length(p0)),
                       upper = rep(NA, length(p0)),
                       dists = rep("norm", length(p0)),
                       log_p = rep(TRUE, length(p0)),
                       types = c(
                           "tnorm", "beta", "gamma", "lnorm", "cauchy",
                           "unif", "norm"
                       )) {
    # C++ type string:
    # TNORM_LU = 1,
    # BETA_LU = 2,
    # GAMMA_L = 3,
    # LNORM_L = 4,
    # CAUCHY = 5,
    # UNIF = 6,
    # NORM = 7,
    if (!all(dists %in% types)) {
        stop("Invalid distribution in 'dists'. Valid types are: ", paste(types, collapse = ", "))
    }

    nparameter <- length(p0)
    parameter_names <- names(p0)

    ordered_index <- order(parameter_names)

    if (is.null(parameter_names)) stop("p0 must be a named numeric vector.")
    prior_list <- vector("list", nparameter)

    for (i in seq_len(nparameter)) {
        j <- ordered_index[i]
        prior_list[[i]] <- switch(dists[j],
            tnorm  = create_param_list(p0[j], p1[j], lower[j], upper[j], 1, log_p[j]),
            beta   = create_param_list(p0[j], p1[j], lower[j], upper[j], 2, log_p[j]),
            gamma  = create_param_list(p0[j], p1[j], lower[j], upper[j], 3, log_p[j]),
            lnorm  = create_param_list(p0[j], p1[j], lower[j], upper[j], 4, log_p[j]),
            cauchy = create_param_list(p0[j], p1[j], lower[j], upper[j], 5, log_p[j]),
            unif   = create_param_list(p0[j], p1[j], lower[j], upper[j], 6, log_p[j]),
            norm   = create_param_list(p0[j], p1[j], lower[j], upper[j], 7, log_p[j]),
            create_param_list(p0[j], p1[j], -Inf, Inf, NA, FALSE) # Default case
        )
    }

    names(prior_list) <- sort(parameter_names)
    prior_list
}
