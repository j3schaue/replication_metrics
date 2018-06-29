###-----------------------------------------------------------------------------###
###-----------------------------------------------------------------------------###
###-----------------------------------------------------------------------------###
### REPLICATION METRICS - T-TEST FUNCTIONS
###
### This script includes functions and simulations related to the use of 
### T-Test as a means for assessing replication using the bootstrap method.
###
###-----------------------------------------------------------------------------###
###-----------------------------------------------------------------------------###
###-----------------------------------------------------------------------------###


##-----------------------------------------------##
## Sourced Scripts
##-----------------------------------------------##
source("./Ttest-pvals/simttest.R")


##-----------------------------------------------##
## T-Test Function Bootstrap Case 1
##-----------------------------------------------##

case1tab<-function(N, theta, powern1, powern2, powern3, rowNames, colNames){
  
  #############################################
  # TAKES: N; Number of simulations
  #        theta; vector of 3 thetas comparing
  #        powern1; 3 samples sizes for each theta given a certain power
  #        powern2; 3 samples sizes for each theta given a certain power 
  #        powern3; 3 samples sizes for each theta given a certain power 
  #        rowNames; Name of each row of the table which should be a theta value
  #        colNames; Name of each column of the table which should be power comparisons
  # RETURNS: 3x3 table
  #############################################
  
tab<-matrix(data = NA, nrow = 3, ncol = 3, dimnames = list(rowNames, colNames))

sim1<- ttestsim(N, theta[1], theta[2], theta[3], powern1[1], powern1[2], powern1[3])
tab[1,1]<-sum(abs(sim1[,1]) > 1.98)/N 
tab[1,2]<-sum(abs(sim1[,2]) > 1.98)/N 
tab[1,3]<-sum(abs(sim1[,3]) > 1.98)/N 

sim2<- ttestsim(N, theta[1], theta[2], theta[3], powern2[1], powern2[2], powern2[3])
tab[2,1]<-sum(abs(sim2[,1]) > 1.98)/N 
tab[2,2]<-sum(abs(sim2[,2]) > 1.98)/N 
tab[2,3]<-sum(abs(sim2[,3]) > 1.98)/N 

sim3<- ttestsim(N, theta[1], theta[2], theta[3], powern3[1], powern3[2], powern3[3])
tab[3,1]<-sum(abs(sim3[,1]) > 1.98)/N 
tab[3,2]<-sum(abs(sim3[,2]) > 1.98)/N 
tab[3,3]<-sum(abs(sim3[,3]) > 1.98)/N 

return(tab)
}

###################
################################
##########################################


N<-1000
theta<-c(0.2,0.5,0.8)
power40<-c(145.6332,23.30131,9.102075)
power60<-c(244.9453,39.19124,15.309079)
power80<-c(392.4541,62.79265,24.528380)
row<-c("Power 80", "Power 60", "Power 40")
col<-c("Theta 0.2/0.5", "Theta 0.2/0.8", "Theta 0.5/0.8")
#Done table
c1table<-case1tab(N,theta, power40,power60, power80, row, col)

boot1<-replicate(100, case1tab(N,theta, power80,power60, power40, row, col))
case1means<-matrix(data = NA, nrow=3, ncol= 3, dimnames = list(c("Power 80", "Power 60", "Power 40"),c("Theta 0.2/0.5", "Theta 0.2/0.8", "Theta 0.5/0.8")))
case1sd<-matrix(data = NA, nrow=3, ncol= 3, dimnames = list(c("Power 80", "Power 60", "Power 40"),c("Theta 0.2/0.5", "Theta 0.2/0.8", "Theta 0.5/0.8")))

case1means[1,1]<-mean(boot1[1, 1,])
case1sd[1,1]<-sd(boot1[1, 1,])
case1means[1,2]<-mean(boot1[1,2,])
case1sd[1,2]<-sd(boot1[1, 2,])
case1means[1,3]<-mean(boot1[1,3,])
case1sd[1,3]<-sd(boot1[1, 3,])

case1means[2,1]<-mean(boot1[2, 1,])
case1sd[2,1]<-sd(boot1[2, 1,])
case1means[2,2]<-mean(boot1[2,2,])
case1sd[2,2]<-sd(boot1[2, 2,])
case1means[2,3]<-mean(boot1[2,3,])
case1sd[2,3]<-sd(boot1[2, 3,])

case1means[3,1]<-mean(boot1[3, 1,])
case1sd[3,1]<-sd(boot1[3, 1,])
case1means[3,2]<-mean(boot1[3,2,])
case1sd[3,2]<-sd(boot1[3, 2,])
case1means[3,3]<-mean(boot1[3,3,])
case1sd[3,3]<-sd(boot1[3, 3,])

case1means
case1sd



##-----------------------------------------------##
## T-Test Function Bootstrap Case 2
##-----------------------------------------------##


case2tab<-function(N, theta, powern1, powern2, powern3, rowNames, colNames){
  
  #############################################
  # TAKES: N; Number of simulations
  #        theta; vector of 3 thetas comparing
  #        powern1; 3 samples sizes for each theta given a certain estimate size
  #        powern2; 3 samples sizes for each theta given a certain estimate size 
  #        powern3; 3 samples sizes for each theta given a certain estimate size 
  #        rowNames; Name of each row of the table which should be a theta value
  #        colNames; Name of each column of the table which should be power comparisons
  # RETURNS: 3x3 table
  #############################################
  
  tab<-matrix(data = NA, nrow = 3, ncol = 3, dimnames = list(rowNames, colNames))
  
  sim1<- ttestsim(N, theta[1], theta[1], theta[1], powern1[1], powern2[1], powern3[1])
  tab[1,1]<-sum(abs(sim1[,1]) > 1.98)/N 
  tab[1,2]<-sum(abs(sim1[,2]) > 1.98)/N 
  tab[1,3]<-sum(abs(sim1[,3]) > 1.98)/N 
  
  sim2<- ttestsim(N, theta[2], theta[2], theta[2], powern1[2], powern2[2], powern3[2])
  tab[2,1]<-sum(abs(sim2[,1]) > 1.98)/N 
  tab[2,2]<-sum(abs(sim2[,2]) > 1.98)/N 
  tab[2,3]<-sum(abs(sim2[,3]) > 1.98)/N 
  
  sim3<- ttestsim(N, theta[3], theta[3], theta[3], powern1[3], powern2[3], powern3[3])
  tab[3,1]<-sum(abs(sim3[,1]) > 1.98)/N 
  tab[3,2]<-sum(abs(sim3[,2]) > 1.98)/N 
  tab[3,3]<-sum(abs(sim3[,3]) > 1.98)/N 
  
  return(tab)
}
##################
#######################
###################################

N<-1000
#theta<-c(0.2,0.5,0.8)
power40<-c(145.6332,23.30131,9.102075)
power60<-c(244.9453,39.19124,15.309079)
power80<-c(392.4541,62.79265,24.528380)
row<-c("Theta 0.2", "Theta 0.5", "Theta 0.8")
col<-c("Power 40/60", "Power 40/80", "Power 60/80")
c2table<-case2tab(N,theta, power80,power60, power40, row, col)

boot2<-replicate(100, case2tab(N,theta, power80,power60, power40, row, col))

case2means<-matrix(data = NA, nrow=3, ncol= 3, dimnames = list(c("Theta 0.2", "Theta 0.5", "Theta 0.8"),c("Power 40/60", "Power 40/80", "Power 60/80")))
case2sd<-matrix(data = NA, nrow=3, ncol= 3, dimnames = list(c("Theta 0.2", "Theta 0.5", "Theta 0.8"),c("Power 40/60", "Power 40/80", "Power 60/80")))

case2means[1,1]<-mean(boot2[1, 1,])
case2sd[1,1]<-sd(boot2[1, 1,])
case2means[1,2]<-mean(boot2[1,2,])
case2sd[1,2]<-sd(boot2[1, 2,])
case2means[1,3]<-mean(boot2[1,3,])
case2sd[1,3]<-sd(boot2[1, 3,])

case2means[2,1]<-mean(boot2[2, 1,])
case2sd[2,1]<-sd(boot2[2, 1,])
case2means[2,2]<-mean(boot2[2,2,])
case2sd[2,2]<-sd(boot2[2, 2,])
case2means[2,3]<-mean(boot2[2,3,])
case2sd[2,3]<-sd(boot2[2, 3,])

case2means[3,1]<-mean(boot2[3, 1,])
case2sd[3,1]<-sd(boot2[3, 1,])
case2means[3,2]<-mean(boot2[3,2,])
case2sd[3,2]<-sd(boot2[3, 2,])
case2means[3,3]<-mean(boot2[3,3,])
case2sd[3,3]<-sd(boot2[3, 3,])

case2means
case2sd
