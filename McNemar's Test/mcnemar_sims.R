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
  
  pchisq(mc.stat, 1, lower.tail=F) # obtain the p-value for McNemar's test
}

mc_case <- function(N, theta1, theta2, n1, n2, k){
  #############################################
  # TAKES: N; number of runs
  #        theta1; a vector of means (original)
  #        theta2; a vector of means (replication)
  #        n1; sample size (original)
  #        n2; sample size (replication)
  #        k; number of study pairs
  #
  # RETURNS: proportion of N runs where the test
  #          rejects the null in favor of the
  #          alternative.
  #############################################
  
  pVal <- replicate(N, mcnemar_fun(theta1, theta2, n1, n2, k))
  
    #if(theta1 == theta2){
     # 1- length(which(pVal <=.05))/N
    #} else
    #{
      length(which(pVal <=.05))/N

    #}
  
}

# Case 1: Original and replication studies have different true effects but all studies are equally
# powered. Over 10,000 runs of the mcnemar_fun with different combinations of
# effect size (e.g. small original effect & large replication effect), we find that McNemar's Test
# will rarely reject (~7%) the null hypothesis in favor of the alternative despite the difference in
# true effect size.

# All studies have 80% power. Original studies have a true effect of 0.2 and replication studies
# have a true effect of 0.8.

sl80 <- replicate(100,mc_case(1000, 0.2, 0.8, 392.4541, 24.52838, 100))
sm80 <- replicate(100,mc_case(1000, 0.2, 0.5, 392.4541, 62.79265, 100))
ml80 <- replicate(100,mc_case(1000, 0.5, 0.8, 62.79265, 24.52838, 100))

mean(sl80); sd(sl80)
mean(sm80); sd(sm80)
mean(ml80); sd(ml80)

# All studies have 60% power. Original studies have a true effect of 0.5 and replication studies 
# have true effect of 0.8.

sl60 <- replicate(100, mc_case(1000, 0.2, 0.8, 244.9453, 15.309079, 100))
sm60 <- replicate(100, mc_case(1000, 0.2, 0.5, 244.9453, 39.19124, 100))
ml60 <- replicate(100, mc_case(1000, 0.5, 0.8, 39.19124, 15.309079, 100))

mean(sl60); sd(sl60)
mean(sm60); sd(sm60)
mean(ml60); sd(ml60)

# All studies have 40% power. Original studies have a true effect of 0.2 and replication studies
# have a true effect of 0.8.

sl40 <- replicate(100, mc_case(1000, 0.2, 0.8, 145.6332, 9.102075, 100))
sm40 <- replicate(100, mc_case(1000, 0.2, 0.5, 145.6332, 23.30131, 100))
ml40 <- replicate(100, mc_case(1000, 0.5, 0.8, 23.30131, 9.102075, 100))

mean(sl40); sd(sl40)
mean(sm40); sd(sm40)
mean(ml40); sd(ml40)

# Case 2: Original and replication studies have the same true effects but vary in power
# (i.e. vary in sample size). Essentially what we're seeing here is that McNemar's Test 
# will almost always fail to reject the null hypothesis. 

# All studies have the same true effect but originals are powered at 40% and replications
# at 80%.

lh2 <- replicate(100, mc_case(1000, 0.2, 0.2, 145.6332, 392.4541, 100))
lh5 <- replicate(100, mc_case(1000, 0.5, 0.5, 23.30131, 62.79265, 100))
lh8 <- replicate(100, mc_case(1000, 0.8, 0.8, 9.102075, 24.52838, 100))

mean(lh2); sd(lh2)
mean(lh5); sd(lh5)
mean(lh8); sd(lh8)

# All studies have the same true effect but originals are powered at 60% and replications
# at 80%.

mh2 <- replicate(100, mc_case(1000, 0.2, 0.2, 244.9453, 392.4541, 100))
mh5 <- replicate(100, mc_case(1000, 0.5, 0.5, 39.19124, 62.79265, 100))
mh8 <- replicate(100, mc_case(1000, 0.8, 0.8, 15.309079, 24.52838, 100))

mean(mh2); sd(mh2)
mean(mh5); sd(mh5)
mean(mh8); sd(mh8)

# All studies have the same true effect but originals are powered at 40% and replications
# at 60%.

lm2 <- replicate(100, mc_case(1000, 0.2, 0.2, 145.6332, 244.9453, 100))
lm5 <- replicate(100, mc_case(1000, 0.5, 0.5, 23.30131, 39.19124, 100))
lm8 <- replicate(100, mc_case(1000, 0.8, 0.8, 9.102075, 15.309079, 100))

mean(lm2); sd(lm2)
mean(lm5); sd(lm5)
mean(lm8); sd(lm8)


