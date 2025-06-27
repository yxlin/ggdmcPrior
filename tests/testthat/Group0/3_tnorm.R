# q(save = "no")
cat("\n\n-------------------- Testing tnorm --------------------")
rm(list = ls())
pkg <- c("ggdmcPrior")
suppressPackageStartupMessages(tmp <- sapply(pkg, require, character.only = TRUE))
cat("\nWorking directory: ", getwd(), "\n")

n <- 1e5
p0 <- 0
p1 <- 1
lower <- 0
upper <- Inf
rtnorm_dat <- ggdmcPrior::rtnorm(n, p0, p1, lower, upper)


## dtnorm example
x <- seq(-5, 5, length.out = 1e3)
dtnorm_dat <- dtnorm(x, p0, p1, lower = -2, upper = 2, log_p = FALSE)


q <- seq(-5, 5, length.out = 1e3)
ptnorm_dat <- ptnorm(q, p0, p1, lower = -2, upper = 3, lower_tail = TRUE, log_p = FALSE)


cex_lab <- 2
cex_axis <- 1.5
line_width <- 2

pdf(file = "tests/testthat/Group0/tnorm.pdf")

hist(rtnorm_dat,
    breaks = "fd", freq = FALSE, xlab = "", cex.lab = cex_lab, cex.axis = cex_axis,
    main = ""
)

plot(x, dtnorm_dat,
    type = "l", lwd = line_width, xlab = "", ylab = "Density",
    cex.lab = cex_lab, cex.axis = cex_axis, main = ""
)

plot(q, ptnorm_dat,
    type = "l", lwd = line_width, xlab = "", ylab = "Density",
    cex.lab = cex_lab, cex.axis = cex_axis, main = ""
)

dev.off()
