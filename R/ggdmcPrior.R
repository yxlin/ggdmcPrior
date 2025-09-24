#' ggdmc module for standard and truncated distributions
#'
#' Provides tools for specifying and evaluating standard and truncated
#' probability distributions, with support for log-space computation and
#' joint distribution specification. It enables Bayesian computation for
#' cognition models and includes utilities for density calculation, sampling,
#' and visualisation, facilitating prior distribution specification and model
#' assessment in hierarchical Bayesian frameworks.
#'
#' @keywords package
#' @name ggdmcPrior
#' @keywords internal
#' @author  Yi-Shin Lin <yishinlin001@gmail.com>
#' @importFrom Rcpp evalCpp
#' @useDynLib ggdmcPrior
"_PACKAGE"
NULL

## Helper functions ------
#' @importFrom stats setNames
.merge_priors <- function(
    location, scale, loc_name = "loc_", sca_name = "sca_") {
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
        {
            # 1: Truncated normal (tnorm)
            lb <- pmax(p$lower, p[[1]] - 3 * p[[2]])
            ub <- pmin(p$upper, p[[1]] + 3 * p[[2]])
            seq(lb, ub, length.out = npoint)
        },
        {
            # 2: Beta (beta)
            # The Beta distribution is defined on the interval [0, 1].  It's a
            # probability distribution describing the probability of
            # proportions or fractions.  Therefore, it doesn't make sense to
            # evaluate it outside of that range
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

## External functions --------------------

#' Visualise Distributions
#'
#' Plots density curves for specified distributions to help visualise
#' their shape and domain.
#'
#' @param p_prior A list of distribution specifications. Each element should be
#' a list containing:
#'   \describe{
#'     \item{\code{dist}}{A character string specifying the distribution type.
#'       Supported values include: \code{"tnorm"}, \code{"beta"}, \code{"gamma"},
#'       \code{"lnorm"}, \code{"cauchy"}, \code{"unif"}, and \code{"norm"}.}
#'     \item{\code{p0}}{The first parameter of the distribution.}
#'     \item{\code{p1}}{The second parameter of the distribution (if applicable).}
#'     \item{\code{lower}}{Optional lower bound (used for truncated distributions).}
#'     \item{\code{upper}}{Optional upper bound (used for truncated distributions).}
#'     \item{\code{log_p}}{Logical indicating whether to compute log-densities.}
#'   }
#'
#' @param font_size Numeric. Base font size for plot labels. Defaults to 5.
#' @param cex Numeric. Scaling factor for plot elements. Defaults to 5.
#' @param return_data Logical. If \code{TRUE}, returns the computed density data instead
#' of plotting. Defaults to \code{FALSE}.
#' @param auto_layout Logical. If TRUE (default), automatically choose the number of columns
#'   based on the number of panels.
#' @param panels_per_col Integer. Approximate number of panels per column before
#'   starting a new column (default = 5).
#' @param max_cols Integer. Maximum number of columns to allow in the layout (default = 4).
#' @param ncol_override Integer or NULL. If set, forces the number of columns in the layout.

#' @return If \code{return_data = FALSE} (default), a lattice plot object is returned
#' displaying the density curves for each prior. If \code{return_data = TRUE}, a data
#' frame is returned with the following columns:
#' \itemize{
#'   \item \code{x}: Numeric vector of x-values generated for each prior using a heuristic.
#'   \item \code{y}: Corresponding density (or log-density) values.
#'   \item \code{gp}: Group label or parameter name for each distribution.
#' }
#'
#' @details
#' This function:
#' \itemize{
#'   \item Automatically determines appropriate x-axis ranges based on each distribution's properties.
#'   \item Handles both truncated and unbounded distributions.
#'   \item Supports all distribution types implemented in the package.
#' }
#'
#' For truncated distributions, the density is plotted only within the specified bounds.
#' A heuristic is used to generate axis limits and labels using the internal
#' \code{generate_x_value} function.
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
plot_prior <- function(p_prior,
                       font_size = 5,
                       cex = 5,
                       return_data = FALSE,
                       # auto-layout controls
                       auto_layout = TRUE,
                       panels_per_col = 5, # ~5 panels per column before adding another
                       max_cols = 4, # cap columns so labels stay readable
                       ncol_override = NULL # set to a number to force columns
) {
    # plot_prior <- function(p_prior, font_size = 5, cex = 5, return_data = FALSE)
    d <- generate_prior_data(p_prior)

    # how many panels?
    n_panels <- length(unique(d$gp))

    # decide layout: lattice expects c(ncol, nrow)
    if (!is.null(ncol_override)) {
        ncol <- as.integer(ncol_override)
    } else if (auto_layout) {
        # Base rule: 1 col up to 5 panels, 2 cols up to 10, 3 up to 15, etc.
        ncol <- max(1L, ceiling(n_panels / panels_per_col))
        ncol <- min(ncol, max_cols)
    } else {
        ncol <- 1L
    }
    nrow <- ceiling(n_panels / ncol)
    lay <- c(ncol, nrow)

    p0 <- lattice::xyplot(y ~ x | gp,
        data = d,
        type = "l", # Line plot
        xlab = "", # Empty x-axis label
        ylab = "Density", # y-axis label
        layout = lay, # c(1, length(unique(d$gp))),
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


#' Build a Joint Prior Distribution
#'
#' \code{BuildPrior} sets up a joint distribution of the prior. Each model
#' parameter is assigned one probability distribution.
#
#' \code{p0} and \code{p1} refer to the first and second parameters.
#' I use the convention of the 0-based index to work with the C++ and the
#' Python sister package, 'pydmc' (coming soon). \code{p0} must comes with
#' parameter names.
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
