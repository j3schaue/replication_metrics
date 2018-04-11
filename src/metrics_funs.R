###-----------------------------------------------------------------------------###
###-----------------------------------------------------------------------------###
###-----------------------------------------------------------------------------###
### REPLICATION METRICS COMMON FUNCTIONS LIBRARY
###
### This script brings together common functions used in assessing the 
### properties of replication metrics used by the RPP and RPE.
###
### It includes some functions for running simulations, computing p-values, etc.
###-----------------------------------------------------------------------------###
###-----------------------------------------------------------------------------###
###-----------------------------------------------------------------------------###

##-----------------------------------------------##
## Common Libraries
##-----------------------------------------------##
library(dplyr); library(distr); library(ggplot2)


##-----------------------------------------------##
## Power and p-value functions
##-----------------------------------------------##
power_fun <- function(theta, v){
  
  #############################################
  # TAKES: theta; a vector of means
  #         v; a vector of sampling variances
  #
  # RETURNS: power of design (vector)
  #############################################
  
  return((1 - pnorm(1.96, theta/sqrt(v))) + pnorm(-1.96, theta/sqrt(v))) 
  
}

pval_fun <- function(t, v){
  
  #############################################
  # TAKES: t; a vector of estimates
  #         v; a vector of sampling variances
  #
  # RETURNS: p-value of design (vector)
  # 
  # * Assumes normal dist. w/known variance
  #############################################
  
  return(2 * (1 - pnorm(abs(t)/sqrt(v), 0, 1))) 
  
}

##-----------------------------------------------##
## Simulating k studies
##-----------------------------------------------##
simulate_studies <- function(theta, v, k=NULL){
  
  #############################################
  # TAKES: theta; a vector of means
  #         v; a vector of sampling variances
  #         k; an integer for the # of studies (optional)
  #
  # RETURNS: data frame of (T, v, theta, v, p-value, power) 
  #          drawn from normal dist.
  #############################################
  
  # Resolve arguments
  if(is.null(k)){
    
    k = length(theta)
  
  } else {
    
    if(length(theta) == 1){
      theta = rep(theta, k)
    }
    
    if(length(v) == 1){
      v = rep(v, k)
    }
  
  }
  
  # Check input
  if(length(theta) > 1 & length(theta) != k){ # Invalid 
    
    print("Error: theta does not have length k.")
    return(NULL)

  } else if(length(v) > 1 & length(v) != k){ # Invalid
    
    print("Error: v does not have length k.")
    return(NULL)
    
  } else if((length(theta) > 1 & length(v) > 1) & length(theta) != length(v)){ # Invalid
    
    print("Error: v and theta have different lengths.")
    return(NULL)
    
  } else { # Valid: run simluation and return values
    
    ###-----Run simluation, compute properties, and return it as a DF.
    draws = rnorm(k, theta, sqrt(v))
    return(
      data.frame(
              t = draws, # draws (T)
              v = v, # variances
              theta = theta, # means
              power = power_fun(theta, v), # power of each study
              p = pval_fun(draws, v) # p-value of each estimate
      )
    )
  
  }
}

simulate_studies(rnorm(10, 0.2, .1), 3/393)


##-----------------------------------------------##
## Standard Themes for ggplot
##-----------------------------------------------##
std_theme = theme(panel.grid.minor = element_blank(),
                  axis.ticks = element_blank())
