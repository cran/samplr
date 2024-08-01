## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(samplr)
library(ggplot2)

## -----------------------------------------------------------------------------
set.seed(1)
# Create a matrix with the means of 15 different Gaussians
names <- rep("mvnorm", 15)
parameters <- list()
for (i in 1:15){
  parameters[[i]] <- list(runif(2) * 18 - 9, diag(2))
}

## -----------------------------------------------------------------------------
weights <- runif(15)
weights <- weights / rep(sum(weights), 15)

## ----dpi=150, fig.align="center"----------------------------------------------
hill_map <- plot_2d_density(
  start = c(-10,-10), size = 20, 
  cellsPerRow = 150, names, parameters, weights
)
print(hill_map)

## ----dpi=150, fig.align="center"----------------------------------------------
iterations = 2**10
MH <- sampler_mh(
  start = c(5,5), distr_name = names, distr_params = parameters, 
  sigma_prop = diag(2) / 8, iterations = iterations, weights = weights
)

MH_df <- data.frame(x = MH[[1]][,1], y = MH[[1]][,2])

hill_map + 
  geom_path(MH_df, mapping = aes(x,y), 
            colour = "red", linetype = "dashed", size = .3) + 
  geom_point(MH_df, mapping = aes(x,y), 
             colour = "white",size =.1)

## ----dpi=150, fig.align="center"----------------------------------------------

MC3 <- sampler_mc3(
  start = c(5,5), distr_name = names, distr_params = parameters, 
  sigma_prop = diag(2) / 8, swap_all = FALSE, 
  iterations = iterations, weights = weights
)
MC3_df <- data.frame(x = MC3[[1]][,1,1], y = MC3[[1]][,2,1])

hill_map + 
  geom_path(MC3_df, mapping = aes(x,y), 
            colour = "red", linetype = "dashed", size = .3) + 
  geom_point(MC3_df, mapping = aes(x,y), 
             colour = "white",size =.1)

## ----dpi=150, fig.align="center"----------------------------------------------
# epsilon = .5, L = 10
HMC_.5 <-  sampler_hmc(
  start = c(5,5), distr_name = names, distr_params = parameters,
  iterations = iterations, weights = weights
)
HMC_.5_df <- data.frame(x = HMC_.5[[1]][,1], y = HMC_.5[[1]][,2])

# epsilon = 1, L = 50
HMC_1 <-  sampler_hmc(
  start = c(5,5), distr_name = names, distr_params = parameters,
  L = 50, iterations = iterations, weights = weights, epsilon = 1
)
HMC_1_df <- data.frame(x = HMC_1[[1]][,1], y = HMC_1[[1]][,2])

hill_map + 
  geom_path(HMC_.5_df, mapping = aes(x,y), 
            colour = "red", linetype = "dashed", size = .3) + 
  geom_point(HMC_.5_df, mapping = aes(x,y), 
             colour = "white",size =.1) + 
  
  geom_path(HMC_1_df, mapping = aes(x,y), 
            colour = "yellow", linetype = "dashed", size = .3) + 
  geom_point(HMC_1_df, mapping = aes(x,y), 
             colour = "white",size =.1)

