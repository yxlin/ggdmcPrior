# q(save = "no")
cat("\n-------------------- Testing BuildPrior etc --------------------\n")
pkg <- c("ggdmcPrior")
suppressPackageStartupMessages(tmp <- sapply(pkg, require, character.only = TRUE))

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
res1 <- dnorm(parameters, p0, 0.1, TRUE)

testthat::expect_equal(res0, res1)
# 1.258647   -1.741353 -188.741353   -1.741353   -3.116353
