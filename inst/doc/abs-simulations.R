## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
require(samplr)
set.seed(123)

trial_stim <- sample(20:25, 10, replace=TRUE)
print(trial_stim)

## -----------------------------------------------------------------------------
abs_model <- Zhu23ABS$new(
  width = 1, n_chains = 3, nd_time = 0.3, 
  s_nd_time = 0.2, lambda = 10, distr_name = 'norm', distr_params = 1
)

## ----results = FALSE----------------------------------------------------------
abs_model$simulate(stopping_rule = 'fixed', 
                   n_sample = 5, trial_stim = trial_stim)

## -----------------------------------------------------------------------------
knitr::kable(abs_model$sim_results)

## -----------------------------------------------------------------------------
start_point <- runif(length(trial_stim), 20, 25)
print(start_point)

## -----------------------------------------------------------------------------
abs_model$reset_sim_results()
abs_model$simulate(stopping_rule = 'fixed', 
                   start_point = start_point, 
                   n_sample = 5, 
                   trial_stim = trial_stim)

## -----------------------------------------------------------------------------
knitr::kable(abs_model$sim_results)

## -----------------------------------------------------------------------------
abs_model$confidence_interval(0.5)

## -----------------------------------------------------------------------------
knitr::kable(abs_model$sim_results)

## -----------------------------------------------------------------------------
abs_model$reset_sim_results()
abs_model$simulate(
  stopping_rule = 'fixed', 
  n_sample = 5, 
  trial_stim = trial_stim, 
  start_point=start_point)$confidence_interval(0.5)
knitr::kable(abs_model$sim_results)

## -----------------------------------------------------------------------------
 custom_post_func <- function(x){
  if (x >= 19 & x < 22){
    return(0.3)
  } else if (x >= 22 & x < 24) {
    return(0.6)
  } else if (x >= 24 & x < 26) {
    return(0.1)
  } else {
    return(0)
  }
}

## -----------------------------------------------------------------------------
custom_func_list <- replicate(
  length(trial_stim), custom_post_func, simplify = FALSE
)
abs_model <- Zhu23ABS$new(
  width = 1, n_chains = 3, nd_time = 0.3, 
  s_nd_time = 0.2, lambda = 10, 
  custom_distr = custom_func_list, custom_start = 23
)
abs_model$simulate(
  stopping_rule = 'fixed', 
  n_sample = 5, 
  trial_stim = trial_stim
)

## -----------------------------------------------------------------------------
knitr::kable(abs_model$sim_results)

## -----------------------------------------------------------------------------
trial_stim <- factor(sample(c('left', 'right'), 10, TRUE))

## ----results=FALSE------------------------------------------------------------
abs_model2 <- Zhu23ABS$new(
  width=1, n_chains = 3, nd_time = 0.3, s_nd_time = 0.2, 
  lambda = 10, distr_name = 'norm', distr_params = 1
)
abs_model2$simulate(
  stopping_rule = 'relative', delta = 4, dec_bdry = 0, 
  discrim = 1, trial_stim = trial_stim
)

## -----------------------------------------------------------------------------
knitr::kable(abs_model2$sim_results)

## -----------------------------------------------------------------------------
start_point <- runif(length(trial_stim), -3, 3)
print(start_point)

## -----------------------------------------------------------------------------
abs_model2$reset_sim_results()
abs_model2$simulate(
  stopping_rule = 'relative', delta = 4, dec_bdry = 0, 
  discrim = 1, trial_stim = trial_stim, start_point = start_point, 
  prior_depend = FALSE
)
knitr::kable(abs_model2$sim_results)

## -----------------------------------------------------------------------------
custom_post_left <- function(x){
  if (x >= -3 & x < -1){
    return(0.25 * x + 0.75)
  } else if (x >= -1 & x < 0) {
    return(-0.25 * x + 0.25)
  } else {
    return (0)
  }
}

custom_post_right <- function(x){
  if (x >= -1 & x < 1){
    return(0.25 * x + 0.25)
  } else if (x >= 1 & x < 3) {
    return(-0.25 * x + 0.75)
  } else {
    return (0)
  }
}

custom_func_list <- lapply(trial_stim, function(stim) ifelse(stim=='left', custom_post_left, custom_post_right))

## -----------------------------------------------------------------------------
abs_model2 <- Zhu23ABS$new(
  width=1, n_chains = 3, nd_time = 0.3, s_nd_time = 0.2, 
  lambda = 10, custom_distr = custom_func_list, custom_start = -0.1
)
abs_model2$simulate(
  stopping_rule = 'relative', delta = 4, dec_bdry = 0, 
  discrim = 1, trial_stim = trial_stim
)

## -----------------------------------------------------------------------------
knitr::kable(abs_model2$sim_results)

