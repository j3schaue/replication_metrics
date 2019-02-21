###-----------------------------------------------------------------------------###
###-----------------------------------------------------------------------------###
###-----------------------------------------------------------------------------###
### REPLICATION METRICS - T-TEST FUNCTIONS
###
### This script includes functions and simulations related to the use of 
### T-Test as a means for assessing replication.
###
###-----------------------------------------------------------------------------###
###-----------------------------------------------------------------------------###
###-----------------------------------------------------------------------------###


##-----------------------------------------------##
## Sourced Scripts
##-----------------------------------------------##
source("./src/metrics_funs.R")


##-----------------------------------------------##
## T-Test Function
##-----------------------------------------------##

ttestsim<-function(N, n, theta1,theta2, theta3, powern1, powern2, powern3){
  
  #############################################
  # TAKES: N; Number of simulations
  #       n; number of pairs
  #       theta1; theta coulmn 1
  #       theta2; theta coulmn 2
  #       theta3; theta coulmn 3
  #       powern1; sample size needed for given power given theta 1
  #       powern2; sample size needed for given power given theta 2
  #       powern3; sample size needed for given power given theta 3
  #
  # RETURNS: Nx3 matrix with t statistics for each column
  #############################################
  
  tmatrix<-matrix(data = NA, nrow = N, ncol = 3)
  for (k in 1:N){
    
    t1<-rep(theta1, n) # theta values column 1
    sim_1<-simulate_studies(t1, 2/(powern1)) # sample size needed for given power depending on theta1 
    t2<-rep(theta2, n) # theta values column 2
    sim_2<-simulate_studies(t2, 2/(powern2)) # samples size needed for given power depending on theta2
    t3<-rep(theta3, n) # theta values column 3
    sim_3<-simulate_studies(t3, 2/(powern3)) # samples size needed for given power depending on theta3
    
    #difference in each pvalue
    diff_1<-sim_1$p - sim_2$p 
    diff_2<-sim_1$p - sim_3$p 
    diff_3<-sim_2$p - sim_3$p 
    
    # find t statistics
    # single t statistics for given disparity
    ts1<- mean(diff_1)/(sd(diff_1)/sqrt(n))
    ts2<- mean(diff_2)/(sd(diff_2)/sqrt(n))
    ts3<- mean(diff_3)/(sd(diff_3)/sqrt(n))
    
    tmatrix[k,1]<-ts1
    tmatrix[k,2]<-ts2
    tmatrix[k,3]<-ts3
    
  }
  
  return(tmatrix)
  
}

##-----------------------------------------------##
## Case 1 different thetas but same power
##-----------------------------------------------##
# same for both case 1 & 2
N<- 10000
n<-100


tab<-matrix(data = NA, nrow = 3, ncol = 3, dimnames = list(c("Power 80", "Power 60", "Power 40"), c("Org. 0.2 Rep. 0.5", "Org. 0.2 Rep. 0.8", "Org. 0.5 Rep. 0.8")))
# 80 % power with different sample size and theta (theta1 matches n1, etc.)
theta1<-0.2
theta2<-0.5
theta3<-0.8

n1<-392.4541
n2<-62.79265
n3<-24.52838

# running simulation
power80diff<-ttestsim(N, n, theta1, theta2, theta3, n1, n2, n3)
# putting simulation into a table 
# take the column from power80diff (comparing each thetaX with nX) and finds which test statistic is significant 
# takes the sum total of significant test statistics over all test statistics to get false acceptance rates
# this is the probability that we do not reject
tab[1,]<-apply(power80diff, 2, FUN = function(x) sum(abs(x) > qt(0.975, df =n-1))/N)

# 60% power with different sample size and thetas
theta1<-0.2
theta2<-0.5
theta3<-0.8

n1<-244.9453
n2<-39.19124
n3<-15.309079

#running sim
power60diff<-ttestsim(N, n, theta1, theta2, theta3, n1, n2, n3)

# putting sim into table
tab[2,]<-apply(power60diff, 2, FUN = function(x) sum(abs(x) > qt(0.975, df =n-1))/N)


# 40% power with different sample size and thetas

theta1<-0.2
theta2<-0.5
theta3<-0.8

n1<-145.6332
n2<-23.30131
n3<-9.102075

# running sim for 40%
power40diff<-ttestsim(N, n, theta1, theta2, theta3, n1, n2, n3)
# putting sim into table
tab[3,]<-apply(power40diff, 2, FUN = function(x) sum(abs(x) > qt(0.975, df =n-1))/N)

tab


##-----------------------------------------------##
## Case 2 same thetas with different powers
##-----------------------------------------------##

tab2<-matrix(data = NA, nrow = 3, ncol = 3, dimnames = list(c( "0.2", "0.5", "0.8"), c("Power 40/Power 60", "Power 40/Power 80", "Power 60/Power 80")))
# theta 0.2 with power 40,60,80
theta1<-0.2
theta2<-0.2
theta3<-0.2

n1<-145.6332
n2<-244.9453
n3<-392.4541

# simulation for theta=0.2 with different powers
thetasmall<-ttestsim(N, n,  theta1, theta2, theta3, n1, n2, n3)

tab2[1,]<-apply(thetasmall, 2, FUN = function(x) sum(abs(x) > qt(0.975, df =n-1))/N)

# theta 0.5 with power 40,60,80
theta1<-0.5
theta2<-0.5
theta3<-0.5

n1<-23.30131
n2<-39.19124
n3<-62.79265

#sim for theta=0.5 with different powers
thetamed<-ttestsim(N, n, theta1, theta2, theta3, n1, n2, n3)

tab2[2,]<-apply(thetamed, 2, FUN = function(x) sum(abs(x) > qt(0.975, df =n-1))/N)


# theta 0.8 with power 40,60,80
theta1<-0.8
theta2<-0.8
theta3<-0.8

n1<-9.102075
n2<-15.309079
n3<-24.52838

# Sim for theta=0.8
thetalarge<-ttestsim(N, n, theta1, theta2, theta3, n1, n2, n3)

tab2[3,]<-apply(thetalarge, 2, FUN = function(x) sum(abs(x) > qt(0.975, df =n-1))/N)


tab2

##-----------------------------------------------##
## Different n's Case 1 at theta =0.2
##-----------------------------------------------##

# different n's
tablediffpowern<-matrix(data = NA, nrow = 3, ncol = 3, dimnames = list(c("100 n", "50 n", "25 n"), c("Power 40/Power 60", "Power 40/Power 80", "Power 60/Power 80")))
# theta 0.2 with power 40,60,80
theta1<-0.2
theta2<-0.2
theta3<-0.2

n1<-145.6332
n2<-244.9453
n3<-392.4541
n<-100

# simulation for n=100
thetasmalln<-ttestsim(N,n, theta1, theta2, theta3, n1, n2, n3)

tablediffpowern[1,]<-apply(thetasmalln, 2, FUN = function(x) sum(abs(x) > qt(0.975, df =n-1))/N) # prob of test concluding that they are different for different power


# theta 0.2 with power 40,60,80 n=50
n1<-23.30131
n2<-39.19124
n3<-62.79265

n<-50
thetasmallfifty<-ttestsim(N,n, theta1, theta2, theta3, n1, n2, n3)

tablediffpowern[2,]<-apply(thetasmallfifty, 2, FUN = function(x) sum(abs(x) > qt(0.975, df =n-1))/N) # prob of test concluding that they are different for different power

# theta 0.2 with power 40,60,80 n=25
n1<-9.102075
n2<-15.309079
n3<-24.52828

n<-25

thetasmalltwentyfive<-ttestsim(N,n, theta1, theta2, theta3, n1, n2, n3)

tablediffpowern[3,]<-apply(thetasmalltwentyfive, 2, FUN = function(x) sum(abs(x) > qt(0.975, df =n-1))/N) # prob of test concluding that they are different for different power

tablediffpowern
