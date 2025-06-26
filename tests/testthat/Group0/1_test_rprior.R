# q(save = "no")
cat("\n-------------------- Testing rprior  --------------------\n")
pkg <- c("ggdmcPrior")
suppressPackageStartupMessages(tmp <- sapply(pkg, require, character.only = TRUE))

p0 <- c(A = 0.15, B = 0.45, mean_v = 2.25, sd_v = 0.15, t0 = 0.2)

p_prior <- BuildPrior(
    p0 = p0,
    p1 = rep(0.1, 5),
    lower = rep(NA, 5),
    upper = rep(NA, 5),
    dist = rep("tnorm", 5),
    log_p = rep(TRUE, 5)
)


res <- print_prior(p_prior)



rprior(p_prior, seed = 123)
rprior(p_prior, seed = 456)
rprior(p_prior, seed = 789, verbose = TRUE)
rprior(p_prior)
rprior(p_prior, 2)

n_subject <- 10000
res <- rprior(p_prior, n = n_subject)
attr(res, "dimnames") <- list(names(p_prior), 1:n_subject)

pnames <- names(p_prior)
pdf("rtnorm.pdf")
plot_prior(p_prior)

par(mfrow = c(2, 3))
for (i in seq_len(length(p_prior))) {
    hist(res[i, ],
        breaks = "fd", freq = FALSE, border = NA, col = "lightblue", main = pnames[i],
        xlab = paste0(pnames[i], " values")
    )
}
par(mfrow = c(1, 1))
dev.off()


# Use the beta distribution to create uniform densities
# lower and upper set the bounds. If lower is NA, it will be set to 0.
# If upper is NA, it will be set to 1.
p_prior <- BuildPrior(
    p0 = c(A = 1, B = 1, mean_v = 1, sd_v = 1, t0 = 1),
    p1 = rep(1, 5),
    lower = rep(0, 5),
    upper = 1:5,
    dist = rep("beta", 5),
    log_p = rep(TRUE, 5)
)

n_subject <- 10
res <- rprior(p_prior, n = n_subject)
attr(res, "dimnames") <- list(names(p_prior), 1:n_subject)
round(res, 3)

n_subject <- 10000
res <- rprior(p_prior, n = n_subject)
pnames <- names(p_prior)
pdf("rbeta.pdf")
par(mfrow = c(2, 3))
for (i in seq_len(length(p_prior))) {
    hist(res[i, ],
        breaks = "fd", freq = FALSE, border = NA, col = "lightblue", main = pnames[i],
        xlab = paste0(pnames[i], " values")
    )
}
par(mfrow = c(1, 1))
dev.off()
