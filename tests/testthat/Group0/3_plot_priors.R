# q(save = "no")
cat("\n-------------------- Testing distributions  --------------------\n")
pkg <- c("ggdmcPrior")
suppressPackageStartupMessages(tmp <- sapply(pkg, require, character.only = TRUE))

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

pdf("tests/testthat/Group0/all_priors.pdf")
print(plot_prior(tnorm_prior, font_size = 2.5, cex = 2.5))
print(plot_prior(gamma_prior, font_size = 2.5, cex = 2.5))
print(plot_prior(lnorm_prior, font_size = 2.5, cex = 2.5))
print(plot_prior(cauchy_prior, font_size = 2.5, cex = 2.5))
print(plot_prior(unif_prior, font_size = 2.5, cex = 2.5))
print(plot_prior(norm_prior, font_size = 2.5, cex = 2.5))
dev.off()


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
