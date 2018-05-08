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

ttestsim<-function(N, theta1,theta2, theta3, powern1, powern2, powern3){
  
  #############################################
  # TAKES: N; Number of simulations
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
    
    t1<-rep(theta1, 100) # theta values column 1
    sim_1<-simulate_studies(t1, 2/(powern1)) # sample size needed for given power depending on theta1 
    t2<-rep(theta2, 100) # theta values column 2
    sim_2<-simulate_studies(t2, 2/(powern2)) # samples size needed for given power depending on theta2
    t3<-rep(theta3,100) # theta values column 3
    sim_3<-simulate_studies(t3, 2/(powern3)) # samples size needed for given power depending on theta3
    
    #difference in each pvalue
    diff_1<-sim_1$p - sim_2$p 
    diff_2<-sim_1$p - sim_3$p 
    diff_3<-sim_2$p - sim_3$p 
    
    # find t statistics
    # single t statistics for given disparity
    ts1<- mean(diff_1)/(sd(diff_1)/10)
    ts2<- mean(diff_2)/(sd(diff_2)/10)
    ts3<- mean(diff_3)/(sd(diff_3)/10)
    
    tmatrix[k,1]<-ts1
    tmatrix[k,2]<-ts2
    tmatrix[k,3]<-ts3
    
  }
  
  return(tmatrix)
  
}

##-----------------------------------------------##
## Case 1 different thetas but same power
##-----------------------------------------------##

N<- 10000
tab<-matrix(data = NA, nrow = 3, ncol = 3, dimnames = list(c("Power 80", "Power 60", "Power 40"), c("Org. 0.2 Rep. 0.5", "Org. 0.2 Rep. 0.8", "Org. 0.5 Rep. 0.8")))
# 80 % power
theta1<-0.2
theta2<-0.5
theta3<-0.8

n1<-393
n2<-63
n3<-25

power80diff<-ttestsim(N, theta1, theta2, theta3, n1, n2, n3)

tab[1,1]<-sum(abs(power80diff[,1]) > 1.98)/N # prob of test concluding that they are different for small and meduim differences
tab[1,2]<-sum(abs(power80diff[,2]) > 1.98)/N # prob of test concluding that they are different for small and large differences
tab[1,3]<-sum(abs(power80diff[,3]) > 1.98)/N # prob of test concluding that they are different for medium and large differences

# 60% power
theta1<-0.2
theta2<-0.5
theta3<-0.8

n1<-245
n2<-40
n3<-16

power60diff<-ttestsim(N, theta1, theta2, theta3, n1, n2, n3)

tab[2,1]<-sum(abs(power60diff[,1]) > 1.98)/N # prob of test concluding that they are different for small and meduim differences
tab[2,2]<-sum(abs(power60diff[,2]) > 1.98)/N # prob of test concluding that they are different for small and large differences
tab[2,3]<-sum(abs(power60diff[,3]) > 1.98)/N # prob of test concluding that they are different for medium and large differences

# 40% power

theta1<-0.2
theta2<-0.5
theta3<-0.8

n1<-147
n2<-24
n3<-10

power40diff<-ttestsim(N, theta1, theta2, theta3, n1, n2, n3)

tab[3,1]<-sum(abs(power40diff[,1]) > 1.98)/N # prob of test concluding that they are different for small and meduim differences
tab[3,2]<-sum(abs(power40diff[,2]) > 1.98)/N # prob of test concluding that they are different for small and large differences
tab[3,3]<-sum(abs(power40diff[,3]) > 1.98)/N # prob of test concluding that they are different for medium and large differences

tab


##-----------------------------------------------##
## Case 2 same thetas with different powers
##-----------------------------------------------##

tab2<-matrix(data = NA, nrow = 3, ncol = 3, dimnames = list(c( "0.2", "0.5", "0.8"), c("Power 40/Power 60", "Power 40/Power 80", "Power 60/Power 80")))
# theta 0.2 with power 40,60,80
theta1<-0.2
theta2<-0.2
theta3<-0.2

n1<-147
n2<-245
n3<-393

thetasmall<-ttestsim(N, theta1, theta2, theta3, n1, n2, n3)

tab2[1,1]<-sum(abs(thetasmall[,1]) > 1.98)/N # prob of test concluding that they are different for 40 and 60 power
tab2[1,2]<-sum(abs(thetasmall[,2]) > 1.98)/N # prob of test concluding that they are different for 40 and 80 power
tab2[1,3]<-sum(abs(thetasmall[,3]) > 1.98)/N # prob of test concluding that they are different for 60 and 80 power

# theta 0.5 with power 40,60,80
theta1<-0.5
theta2<-0.5
theta3<-0.5

n1<-24
n2<-40
n3<-63

thetamed<-ttestsim(N, theta1, theta2, theta3, n1, n2, n3)

tab2[2,1]<-sum(abs(thetamed[,1]) > 1.98)/N # prob of test concluding that they are different for 40 and 60 power
tab2[2,2]<-sum(abs(thetamed[,2]) > 1.98)/N # prob of test concluding that they are different for 40 and 80 power
tab2[2,3]<-sum(abs(thetamed[,3]) > 1.98)/N # prob of test concluding that they are different for 60 and 80 power

# theta 0.8 with power 40,60,80
theta1<-0.8
theta2<-0.8
theta3<-0.8

n1<-10
n2<-16
n3<-25

thetalarge<-ttestsim(N, theta1, theta2, theta3, n1, n2, n3)

tab2[3,1]<-sum(abs(thetalarge[,1]) > 1.98)/N # prob of test concluding that they are different for 40 and 60 power
tab2[3,2]<-sum(abs(thetalarge[,2]) > 1.98)/N # prob of test concluding that they are different for 40 and 80 power
tab2[3,3]<-sum(abs(thetalarge[,3]) > 1.98)/N # prob of test concluding that they are different for 60 and 80 power

tab2

######################################################
# test power is 50,60,90


tab3<-matrix(data = NA, nrow = 3, ncol = 3, dimnames = list(c( "0.2", "0.5", "0.8"), c("Power 50/Power 60", "Power 50/Power 90", "Power 60/Power 90")))
# theta 0.2 with power 50,60,90
theta1<-0.2
theta2<-0.2
theta3<-0.2

n1<-193
n2<-245
n3<-526

thetasmall<-ttestsim(N, theta1, theta2, theta3, n1, n2, n3)

tab3[1,1]<-sum(abs(thetasmall[,1]) > 1.98)/N # prob of test concluding that they are different for 40 and 60 power
tab3[1,2]<-sum(abs(thetasmall[,2]) > 1.98)/N # prob of test concluding that they are different for 40 and 80 power
tab3[1,3]<-sum(abs(thetasmall[,3]) > 1.98)/N # prob of test concluding that they are different for 60 and 80 power

# theta 0.5 with power 40,60,80
theta1<-0.5
theta2<-0.5
theta3<-0.5

n1<-31
n2<-40
n3<-85

thetamed<-ttestsim(N, theta1, theta2, theta3, n1, n2, n3)

tab3[2,1]<-sum(abs(thetamed[,1]) > 1.98)/N # prob of test concluding that they are different for 40 and 60 power
tab3[2,2]<-sum(abs(thetamed[,2]) > 1.98)/N # prob of test concluding that they are different for 40 and 80 power
tab3[2,3]<-sum(abs(thetamed[,3]) > 1.98)/N # prob of test concluding that they are different for 60 and 80 power

# theta 0.8 with power 40,60,80
theta1<-0.8
theta2<-0.8
theta3<-0.8

n1<-13
n2<-16
n3<-33

thetalarge<-ttestsim(N, theta1, theta2, theta3, n1, n2, n3)

tab3[3,1]<-sum(abs(thetalarge[,1]) > 1.98)/N # prob of test concluding that they are different for 40 and 60 power
tab3[3,2]<-sum(abs(thetalarge[,2]) > 1.98)/N # prob of test concluding that they are different for 40 and 80 power
tab3[3,3]<-sum(abs(thetalarge[,3]) > 1.98)/N # prob of test concluding that they are different for 60 and 80 power

tab3

######################################################
# test power is 60,70,80


tab4<-matrix(data = NA, nrow = 3, ncol = 3, dimnames = list(c( "0.2", "0.5", "0.8"), c("Power 60/Power 70", "Power 60/Power 80", "Power 70/Power 80")))
# theta 0.2 with power 40,60,80
theta1<-0.2
theta2<-0.2
theta3<-0.2

n1<-245
n2<-309
n3<-393

thetasmall<-ttestsim(N, theta1, theta2, theta3, n1, n2, n3)

tab4[1,1]<-sum(abs(thetasmall[,1]) > 1.98)/N # prob of test concluding that they are different for 40 and 60 power
tab4[1,2]<-sum(abs(thetasmall[,2]) > 1.98)/N # prob of test concluding that they are different for 40 and 80 power
tab4[1,3]<-sum(abs(thetasmall[,3]) > 1.98)/N # prob of test concluding that they are different for 60 and 80 power

# theta 0.5 with power 40,60,80
theta1<-0.5
theta2<-0.5
theta3<-0.5

n1<-40
n2<-50
n3<-63

thetamed<-ttestsim(N, theta1, theta2, theta3, n1, n2, n3)

tab4[2,1]<-sum(abs(thetamed[,1]) > 1.98)/N # prob of test concluding that they are different for 40 and 60 power
tab4[2,2]<-sum(abs(thetamed[,2]) > 1.98)/N # prob of test concluding that they are different for 40 and 80 power
tab4[2,3]<-sum(abs(thetamed[,3]) > 1.98)/N # prob of test concluding that they are different for 60 and 80 power

# theta 0.8 with power 40,60,80
theta1<-0.8
theta2<-0.8
theta3<-0.8

n1<-16
n2<-20
n3<-25

thetalarge<-ttestsim(N, theta1, theta2, theta3, n1, n2, n3)

tab4[3,1]<-sum(abs(thetalarge[,1]) > 1.98)/N # prob of test concluding that they are different for 40 and 60 power
tab4[3,2]<-sum(abs(thetalarge[,2]) > 1.98)/N # prob of test concluding that they are different for 40 and 80 power
tab4[3,3]<-sum(abs(thetalarge[,3]) > 1.98)/N # prob of test concluding that they are different for 60 and 80 power

tab4
