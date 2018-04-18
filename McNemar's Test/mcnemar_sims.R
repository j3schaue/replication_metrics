###-----------------------------------------------------------------------------###
###-----------------------------------------------------------------------------###
###-----------------------------------------------------------------------------###
### REPLICATION METRICS - MCNEMARS TEST FUNCTIONS
###
### This script includes functions and simulations related to the use of 
### McNemar's Test as a means for assessing replication.
###
###-----------------------------------------------------------------------------###
###-----------------------------------------------------------------------------###
###-----------------------------------------------------------------------------###


##-----------------------------------------------##
## Sourced Scripts
##-----------------------------------------------##
source("./src/metrics_funs.R")

##-----------------------------------------------##
## McNemar's Test Function
##-----------------------------------------------##

mcnemar_fun <- function(theta1, theta2, n1, n2, k){
  
  #############################################
  # TAKES: theta1; a vector of means (original)
  #        theta2; a vector of means (replication)
  #        n1; sample size (original)
  #        n2; sample size (replication)
  #        k; number of study pairs
  #
  # RETURNS: p-value of McNemar's Test
  #############################################
  
  
  orig.studies <- simulate_studies(rep(theta1, k), 2/n1) # simulate k original studies
  rep.studies  <- simulate_studies(rep(theta2, k), 2/n2) # simulate k replication studies
  
  # number of study pairs for which original finding is significant and replication finding is not
  b <- length(which(orig.studies$p <=.05 & rep.studies$p > .05)) 
  
  # number of study pairs for which original finding is not significant and replication finding is
  c <- length(which(orig.studies$p > .05 & rep.studies$p <= .05))
  
  mc.stat <- ((b-c)^2)/(b+c) # calculate the McNemar's test statistic
  
  pchisq(mc.stat, 1) # obtain the p-value for McNemar's test
}

# Case 1: Original and replication studies have different true effects but all studies are equally
# powered (i.e. 80% power). Over 10,000 runs of the mcnemar_fun with different combinations of
# effect size (e.g. small original effect & large replication effect), we find that McNemar's Test
# will rarely reject (~7%) the null hypothesis in favor of the alternative despite the difference in
# true effect size.

chi_sl <- replicate(10000, mcnemar_fun(0.2, 0.8, 393, 25, 100))
chi_sm <- replicate(10000, mcnemar_fun(0.2, 0.5, 393, 64, 100))
chi_ml <- replicate(10000, mcnemar_fun(0.5, 0.8, 64, 25, 100))

# Probability of the test rejecting the null hypothesis in favor of the alternative

length(which(chi_sl <= .05))/length(chi_sl) 
length(which(chi_sm <= .05))/length(chi_sm)
length(which(chi_ml <= .05))/length(chi_ml)


# Case 2: Original and replication studies have the same true effects but vary in power
# (i.e. vary in sample size). 


test_fun <- function(theta, c, k, a, b){
  orig.n <- runif(k, a, b)
  
  mcnemar_fun(theta, theta, orig.n, c*orig.n, k)
}

######################## IN PROGRESS - DON'T LOOK AT IT ###########################

# You may see a trend in the data that alerts you to something that you want to pursue further. 
# E.g. You might see something when the power is really low - interesting phenomenon







# Case 1: Different thetas. Increasing replication sample size.

mc_sim <- function(theta1, theta2, k, n1){

orig.studies <- simulate_studies(rnorm(k, theta1, .01), 2/n1)
result <- rep(0, 26)

for (i in 5:30){
  
  c <- i/10

  rep.studies <- simulate_studies(rnorm(k, theta2, .1), 2/(c*n1))
  
  p01 <- mean((1-orig.studies$power)*rep.studies$power)
  p10 <- mean(orig.studies$power*(1-rep.studies$power))
  
  diff <- p10 - p01
  disc <- p10 + p01
  
  pow.mc <- pnorm((diff*sqrt(k) - 1.96*sqrt(disc))/sqrt(disc - (diff^2)))
  
  result[i] <- pow.mc
}
  return(result)
}

mc_sim(0.2, 0.5, 100, 393) #n=393 is the sample size needed for 80% power when effect is "small"
mc_sim(0.2, 0.8, 100, 393)
mc_sim(0.5, 0.8, 100, 64) #n=64 is the sample size needed for 80% power when effect is "medium"

mc_sim(0.5, 0.2, 100, 64)
mc_sim(0.8, 0.5, 100, 25)
mc_sim(0.8, 0.2, 100, 25)

mc_sim(0.2, 0.2, 100, 393)

test <- data.frame(c=seq(0.5, 3, by=.1), mc.pow = mc_sim(0.2, 0.2, 100, 393))

###########################
c <- .5
theta1 <- 0.2
theta2 <- 0.2
k <- 100

orig.studies <- simulate_studies(rnorm(k, theta1, .1), 2/393)
rep.studies <- simulate_studies(rnorm(k, theta2, .1), 2/(c*393))

test.sn <- ifelse(orig.studies$p <= .05 & rep.studies$p > .05, 1, 0)
test.ns <- ifelse(orig.studies$p > .05 & rep.studies$p <= .05, 1, 0)


p01 <- mean((1-orig.studies$power)*rep.studies$power) 
p10 <- mean(orig.studies$power*(1-rep.studies$power))

p10 ; mean(test.sn)
p01 ; mean(test.ns)

diff <- p10 - p01
disc <- p10 + p01

pow.mc <- pnorm((diff*sqrt(k) - 1.96*sqrt(disc))/sqrt(disc - (diff^2)))
pow.mc

############################

theta <- 0.2
k <- 100
c <- 0.5

test <- function(c){

orig.studies <- simulate_studies(rnorm(100, 0.2, .1), 2/393)


rep.power <- power_fun(orig.studies$theta, (2/(c*393)))

mean(orig.studies$power) ; mean(rep.power)

p01 <- mean((1-orig.studies$power)*rep.power)
p10 <- mean(orig.studies$power*(1-rep.studies$power))

diff <- p10 - p01
disc <- p10 + p01

pow.mc <- pnorm((diff*sqrt(100) - 1.96*sqrt(disc))/sqrt(disc - (diff^2)))
pow.mc
}

test(0.5) ; test(0.2, 100, 0.8) ; test(0.2, 100, 1) ; test(0.2, 100, 1.5); test(0.2, 100, 3)

test.df <- data.frame(c=seq(.1, 3, .1), mc.pow = sapply(seq(.1, 3, .1), test))

test.df[test.df$c==0.5,] ; test(.5)
test.df[test.df$c==0.8,] ; test(.8)
test.df[test.df$c==1,] ; test(1)




