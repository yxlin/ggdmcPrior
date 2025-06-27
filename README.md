# ggdmcPrior
ggdmcPrior provides functions for specifying and evaluating standard distributions, designed to work with the ggdmc package. It supports Bayesian computation and includes utilities for density calculation, sampling, and visualisation.

# Getting Started
The package is mainly to support ggdmc, so you can use it together with other ggdmc supporting packages.

```
cat("\n--------- Printing the joint prior distribution in C++-------\n")
p0 <- c(A = 0.15, B = 0.45, mean_v = 2.25, sd_v = 0.15, t0 = 0.2)
p1 <- rep(0.1, 5)
names(p1) <- names(p0)


p_prior <- BuildPrior(
    p0 = p0,
    p1 = p1,
    lower = rep(NA, 5),
    upper = rep(NA, 5),
    log_p = rep(TRUE, 5),
    dist = c("tnorm", "tnorm", "tnorm", "norm", "tnorm")
)


prior_S4 <- set_priors(p_prior = p_prior)
names(p_prior)

# Test print -----------------
res <- print_prior(p_prior)

# Test dprior -----------------
parameters <- seq(0.1, 0.5, 0.1)
res0 <- dprior(p_prior, parameters)

set.seed(123)
result1 <- rprior(p_prior, 1)
set.seed(123)
result2 <- rprior(p_prior, 2)

# Use the beta distribution to create uniform densities
# lower and upper set the bounds. If lower is NA, it will be set to 0.
# If upper is NA, it will be set to 1.
p_prior <- BuildPrior(
    p0 = c(A = 1, B = 1, mean_v = 1, sd_v = 1, t0 = 1),
    p1 = rep(1, 5),
    lower = rep(0, 5),
    upper = rep(5, 5),
    dist = rep("beta", 5),
    log_p = rep(TRUE, 5)
)
prior_S4 <- set_priors(p_prior = p_prior)

p0 <- plot_prior(p_prior)

```

Six different standard statistical distributions are included in ggdmcPrior.

```
p0 <- c(A = 0.15, B = 0.45, mean_v = 2.25, sd_v = 0.15, t0 = 0.2)

tnorm_prior <- BuildPrior(
    p0 = p0,
    p1 = rep(1, 5),
    lower = rep(0, 5),
    upper = rep(NA, 5),
    dist = rep("tnorm", 5),
    log_p = rep(FALSE, 5)
)


gamma_prior <- BuildPrior(
    p0 = p0,
    p1 = rep(0.1, 5),
    lower = rep(NA, 5),
    upper = rep(NA, 5),
    dist = rep("gamma", 5),
    log_p = rep(FALSE, 5)
)


lnorm_prior <- BuildPrior(
    p0 = p0,
    p1 = rep(0.1, 5),
    lower = rep(NA, 5),
    upper = rep(NA, 5),
    dist = rep("lnorm", 5),
    log_p = rep(FALSE, 5)
)

cauchy_prior <- BuildPrior(
    p0 = p0,
    p1 = rep(0.1, 5),
    lower = rep(NA, 5),
    upper = rep(NA, 5),
    dist = rep("cauchy", 5),
    log_p = rep(FALSE, 5)
)

unif_prior <- BuildPrior(
    p0 = p0,
    p1 = rep(0.1, 5),
    lower = rep(NA, 5),
    upper = rep(NA, 5),
    dist = rep("unif", 5),
    log_p = rep(FALSE, 5)
)

norm_prior <- BuildPrior(
    p0 = p0,
    p1 = rep(0.1, 5),
    lower = rep(NA, 5),
    upper = rep(NA, 5),
    dist = rep("norm", 5),
    log_p = rep(FALSE, 5)
)


print(plot_prior(tnorm_prior, font_size = 2.5, cex = 2.5))
print(plot_prior(gamma_prior, font_size = 2.5, cex = 2.5))
print(plot_prior(lnorm_prior, font_size = 2.5, cex = 2.5))
print(plot_prior(cauchy_prior, font_size = 2.5, cex = 2.5))
print(plot_prior(unif_prior, font_size = 2.5, cex = 2.5))
print(plot_prior(norm_prior, font_size = 2.5, cex = 2.5))



nparameter <- length(p0)
for (i in seq_len(nparameter)) {
    tnorm_prior[[i]]$log_p <- TRUE
    gamma_prior[[i]]$log_p <- TRUE
    lnorm_prior[[i]]$log_p <- TRUE

    cauchy_prior[[i]]$log_p <- TRUE
    unif_prior[[i]]$log_p <- TRUE
    norm_prior[[i]]$log_p <- TRUE
}
res <- print_prior(tnorm_prior)
res <- print_prior(gamma_prior)
res <- print_prior(lnorm_prior)
res <- print_prior(cauchy_prior)
res <- print_prior(unif_prior)
res <- print_prior(norm_prior)

set.seed(123)
parameters_r <- runif(nparameter, 0, 10)

res0 <- sumlogprior(p_prior_r = tnorm_prior, parameters_r = parameters_r)
res1 <- sumlogprior(p_prior_r = gamma_prior, parameters_r = parameters_r)
res2 <- sumlogprior(p_prior_r = lnorm_prior, parameters_r = parameters_r)
res3 <- sumlogprior(p_prior_r = cauchy_prior, parameters_r = parameters_r)
res4 <- sumlogprior(p_prior_r = unif_prior, parameters_r = parameters_r)
res5 <- sumlogprior(p_prior_r = norm_prior, parameters_r = parameters_r)

results <- c(res0, res1, res2, res3, res4, res5)

```

# Prerequisites
R (>= 3.3.0), Rcpp (>= 1.0.7), RcppArmadillo (>= 0.10.7.5.0), ggdmcHeaders, lattice.

See DESCRIPTION for details

# Installation

From CRAN:
```
install.packages("ggdmcPrior")
```