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

##----------------------------------------##
## Common Libraries
##----------------------------------------##
library(dplyr); library(distr); library(ggplot2)


##----------------------------------------##
## Simulating k studies
##----------------------------------------##
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
    return(
      data.frame(
              t = rnorm(k, theta, sqrt(v)), # draws (T)
              v = v, # variances
              theta = theta, # means
              power = (1 - pnorm(1.96, theta/sqrt(v))) + pnorm(-1.96, theta/sqrt(v)), # power of each study
              p = 2 * (1 - pnorm(abs(t)/sqrt(v), 0, 1)) # p-value of each estimate
      )
    )
  
  }
}
simulate_studies(.2, 3/393, 2)


