## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include = FALSE---------------------------------------------------
library(samplr)

## -----------------------------------------------------------------------------
# Create function
customDensity_r <- function(x){
  if (x[1] > 0 && x[1] < 3 && x[2] < 0 && x[2] > -3){
    return (1)
  } else {
    return (0)                    }
}
# Sample
Y <- samplr::sampler_mh(start = c(0,0), custom_density = customDensity_r)

# Plot results
x <- Y[[1]][,1]
y <- Y[[1]][,2]
plot(x,y, xlim = c(-5,5), ylim = c(-5,5))

## -----------------------------------------------------------------------------
Rcpp::cppFunction("double customDensity_cpp(NumericVector x){
                    if (x(0) > 0 && x(0) < 3 && x(1) < 0 && x(1) > -3){
                      return 1;
                    } else {
                      return 0;
                    }
                  }")
X <- bench::mark(
  "CPP Density" = {
    samplr::sampler_mh(
      start = c(0,0), 
      custom_density = customDensity_cpp, 
      sigma_prop = diag(2))
  },
  "R Density" = {
    samplr::sampler_mh(
      start = c(0,0), 
      custom_density = customDensity_r, 
      sigma_prop = diag(2))
  },
  "Supported Density" = {
    samplr::sampler_mh(
      start = c(0,0), 
      distr_name = "mvnorm", 
      distr_params = list(c(0,1), diag(2)), 
      sigma_prop = diag(2))
  },
  check = FALSE,
)
knitr::kable(as.data.frame(X[,c("expression", "min", "median")]))

## -----------------------------------------------------------------------------
bespoke_mh <- function(pdf, start, iterations=1024, sigma_prop){
  # Initialize variables ---------------------------------
  acceptances <- 0
  n_dim <- length(start)
  chain <- matrix(0, nrow=iterations, ncol = n_dim)
  chain[1,] <- start
  probabilities <- c(pdf(start))

  # Run the sampler ------------------------------------------------

  for (i in 2:iterations){
    current_x <- chain[i-1,]
    # generate proposal
    proposal <- mvtnorm::rmvnorm(1, mean = current_x, sigma = sigma_prop)
    # calculate current and proposal probabilities
    prob_curr <- probabilities[i-1]
    prob_prop <- pdf(proposal)
    accept <- FALSE

    # proposal is accepted with probability prob_prop / prob_curr
    if (prob_curr != 0){
      ratio <- prob_prop / prob_curr
      if (ratio >= 1){
        accept <- TRUE
      # The beta parameter (temperature), beta <= 1, 
      # increases the value of the ratio making hotter 
      # chains more likely to accept proposals
      } else if (stats::runif(1) < ratio){
        accept <- TRUE
      }
    } else {
    # in case the current probability is 0 
    # (in which case the ratio cannot be calculated), 
    # the step is accepted if the probability of the proposal is not 0
        if (prob_prop > 0) {
          accept <- TRUE
        }
    }
    
    if (accept){
      chain[i,] <- proposal
      acceptances <- acceptances +  1
      probabilities[i] <- prob_prop
    } else {
      chain[i,] <- current_x
      probabilities[i] <- prob_curr
    }
  
  }

  return(list(
    chain = chain,
    acceptance_ratio = acceptances/(iterations - 1)))
}

X <- bench::mark(
  "Bespoke MH" = {
    bespoke_mh(
      customDensity_r, 
      c(0,0), 
      iterations = 1024, 
      sigma_prop = diag(2)
    )
  }
)
knitr::kable(as.data.frame(X[,c("expression", "min", "median")]))

