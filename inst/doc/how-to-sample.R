## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center"
)

## ----setup--------------------------------------------------------------------
library(samplr)
set.seed(123)

## -----------------------------------------------------------------------------
start = 0
name = "norm"
params = c(0,1)

## -----------------------------------------------------------------------------
proposal_variance <- 1 / 2
MH_samples <- sampler_mh(start, name, params, proposal_variance)

## -----------------------------------------------------------------------------
hist(MH_samples[[1]], freq=FALSE, breaks = 20)
curve(dnorm(x, 0,1), add=TRUE)

## -----------------------------------------------------------------------------
MC3_samples <- sampler_mc3(start, name, params, proposal_variance)

HMC_samples <- sampler_hmc(start, name, params)

## -----------------------------------------------------------------------------

name <- c("norm", "norm")
params <- list(c(-2, 1), c(2, 1))
weights <- c(.4, .6)

## -----------------------------------------------------------------------------
customDensity <- function(x){
  dnorm(x, -2, 1) * 0.4 + dnorm(x, 2, 1) * 0.6
}

## -----------------------------------------------------------------------------
# Sample (more iterations as MH struggles to change hills)
proposal_variance <- 1
MH_samples <- sampler_mh(start, name, params, proposal_variance, iterations = 2**12, weights = weights)
# plot
hist(MH_samples[[1]], freq=FALSE, breaks = 20)
curve(customDensity(x), add=TRUE)

