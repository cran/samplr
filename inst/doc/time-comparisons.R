## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE, 
  fig.align = "left"
  # out.width = "100%"
)

## -----------------------------------------------------------------------------
X <- bench::mark(
    "Metropolis-Hastings" = {samplr::sampler_mh(1, "norm", c(0,1), sigma_prop=1)},
    "MC3" = {samplr::sampler_mc3(1, "norm", c(0,1), sigma_prop=1)},
    "Hamiltonian Monte-Carlo" = {samplr::sampler_hmc(1, "norm", c(0,1))},
    "REC" = {samplr::sampler_rec(1, "norm", c(0,1))},
    "MCHMC" = {samplr::sampler_mchmc(1, "norm", c(0,1), )},
    "MCREC" = {samplr::sampler_mcrec(1, "norm", c(0,1))},
    check = FALSE, iterations = 50
)
knitr::kable(as.data.frame(X[,c("expression", "min", "median")]))

## ----echo=FALSE---------------------------------------------------------------
columns <- c("expression",  "timeit")
tests <- c("Metropolis-Hastings", "MC3")
timeit <- c("6.22ms", "55.13ms")
X <- data.frame(tests, timeit)
knitr::kable(X)

