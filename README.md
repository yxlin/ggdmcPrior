# ggdmcPrior

<!-- Badges -->
[![CRAN Status](https://www.r-pkg.org/badges/version/ggdmcPrior)](https://cran.r-project.org/package=ggdmcPrior)
[![Downloads](https://cranlogs.r-pkg.org/badges/ggdmcPrior)](https://cran.r-project.org/package=ggdmcPrior)
[![License: GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![R-CMD-check](https://github.com/yxlin/ggdmcPrior/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yxlin/ggdmcPrior/actions/workflows/R-CMD-check.yaml)

---

## Overview

**ggdmcPrior** provides tools for specifying and evaluating prior distributions for cognitive models, with support for:

- **Six standard distributions**: truncated normal, normal, gamma, lognormal, Cauchy, and uniform.
- Log-space computation for numerical stability.
- Joint prior specification for multiple parameters.
- Density calculation, random sampling, and visualisation.

The package integrates seamlessly with the [`ggdmc`](https://cran.r-project.org/package=ggdmc) ecosystem, enabling Bayesian inference for choice response time models in a hierarchical framework.

---

## 📦 Prerequisites

- **R** (≥ 3.5.0)  
- **Rcpp** (≥ 1.0.7)  
- **RcppArmadillo** (≥ 0.10.7.5.0)  
- **ggdmcHeaders**  
- **lattice**  

---

## 📥 Installation

From CRAN:
```r
install.packages("ggdmcPrior")
```

## 🚀 Getting Started
Although `ggdmcPrior` can be used independently, it is primarily designed to support the ggdmc workflow.

### Example 1 – Joint Truncated Normal Prior

```r
p0 <- c(A = 0.15, B = 0.45, mean_v = 2.25, sd_v = 0.15, t0 = 0.2)
p1 <- rep(0.1, 5); names(p1) <- names(p0)

p_prior <- BuildPrior(
    p0     = p0,
    p1     = p1,
    lower  = rep(NA, 5),
    upper  = rep(NA, 5),
    log_p  = rep(TRUE, 5),
    dist   = c("tnorm", "tnorm", "tnorm", "norm", "tnorm")
)

print_prior(p_prior)                     # Print definition
dprior(p_prior, seq(0.1, 0.5, 0.1))      # Density calculation
rprior(p_prior, 2)                       # Random sampling
plot_prior(p_prior)                      # Visualisation

```

## 📊 Supported Distributions

```r
p0 <- c(A = 0.15, B = 0.45, mean_v = 2.25, sd_v = 0.15, t0 = 0.2)

tnorm_prior  <- BuildPrior(p0, rep(1, 5),  rep(0, 5),  rep(NA, 5), dist = rep("tnorm",  5))
gamma_prior  <- BuildPrior(p0, rep(0.1,5), rep(NA, 5), rep(NA, 5), dist = rep("gamma",  5))
lnorm_prior  <- BuildPrior(p0, rep(0.1,5), rep(NA, 5), rep(NA, 5), dist = rep("lnorm",  5))
cauchy_prior <- BuildPrior(p0, rep(0.1,5), rep(NA, 5), rep(NA, 5), dist = rep("cauchy", 5))
unif_prior   <- BuildPrior(p0, rep(0.1,5), rep(NA, 5), rep(NA, 5), dist = rep("unif",   5))
norm_prior   <- BuildPrior(p0, rep(0.1,5), rep(NA, 5), rep(NA, 5), dist = rep("norm",   5))

# Visualise all priors
lapply(list(tnorm_prior, gamma_prior, lnorm_prior, cauchy_prior, unif_prior, norm_prior),
       plot_prior, font_size = 2.5, cex = 2.5)

```

## 🔍 Log-Prior Summation Example
```r
set.seed(123)
parameters_r <- runif(length(p0), 0, 10)

sumlogprior(tnorm_prior,  parameters_r)
sumlogprior(gamma_prior,  parameters_r)
sumlogprior(lnorm_prior,  parameters_r)
sumlogprior(cauchy_prior, parameters_r)
sumlogprior(unif_prior,   parameters_r)
sumlogprior(norm_prior,   parameters_r)

```

## 📄 License
GPL (≥ 3)
