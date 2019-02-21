case1tab<-function(N, n, theta, powern1, powern2, powern3, rowNames, colNames){
  
  #############################################
  # TAKES: N; Number of simulations
  #        n; the number or pairs in the simulation
  #        theta; vector of 3 thetas comparing
  #        powern1; 3 samples sizes for each theta given a certain power
  #        powern2; 3 samples sizes for each theta given a certain power 
  #        powern3; 3 samples sizes for each theta given a certain power 
  #        rowNames; Name of each row of the table which should be a theta value
  #        colNames; Name of each column of the table which should be power comparisons
  # RETURNS: 3x3 table
  #############################################
# create empty table  
tab<-matrix(data = NA, nrow = 3, ncol = 3, dimnames = list(rowNames, colNames))
# run simulation for 1st row with different thetas and 1st power level
sim1<- ttestsim(N, n[1], theta[1], theta[2], theta[3], powern1[1], powern1[2], powern1[3])
# fine the proportion of significant findings from the simulation
tab[1,]<-apply(sim1, 2, FUN = function(x) sum(abs(x) > qt(0.975, df =n-1))/N)

# run simulation for 2nd row with different thetas and 2nd power level
sim2<- ttestsim(N, n[2], theta[1], theta[2], theta[3], powern2[1], powern2[2], powern2[3])
# fine the proportion of significant findings from the simulation
tab[2,]<-apply(sim2, 2, FUN = function(x) sum(abs(x) > qt(0.975, df =n-1))/N)

# run simulation for 3rd row with different thetas and 3rd power level
sim3<- ttestsim(N, n[3], theta[1], theta[2], theta[3], powern3[1], powern3[2], powern3[3])
# fine the proportion of significant findings from the simulation
tab[3,]<-apply(sim3, 2, FUN = function(x) sum(abs(x) > qt(0.975, df =n-1))/N)


return(tab)
}

###################
################################
##########################################


N<-1000
n<-c(100,100,100)
theta<-c(0.2,0.5,0.8)
power40<-c(145.6332,23.30131,9.102075)
power60<-c(244.9453,39.19124,15.309079)
power80<-c(392.4541,62.79265,24.52838)
row<-c("Power 80", "Power 60", "Power 40")
col<-c("Theta 0.2/0.5", "Theta 0.2/0.8", "Theta 0.5/0.8")
# c1table<-case1tab(N, n, theta, power40,power60, power80, row, col)
# bootstraps the table of case 1 100 times
boot1<-replicate(100, case1tab(N, n, theta, power80,power60, power40, row, col))
#create empty matrix for mean and sd
case1means<-matrix(data = NA, nrow=3, ncol= 3, dimnames = list(c("Power 80", "Power 60", "Power 40"),c("Theta 0.2/0.5", "Theta 0.2/0.8", "Theta 0.5/0.8")))
case1sd<-matrix(data = NA, nrow=3, ncol= 3, dimnames = list(c("Power 80", "Power 60", "Power 40"),c("Theta 0.2/0.5", "Theta 0.2/0.8", "Theta 0.5/0.8")))

# since have 100 tables of case 1 scenarios in boot1 take the mean and sd of each entry of the 100 tables
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




case2tab<-function(N, n, theta, powern1, powern2, powern3, rowNames, colNames){
  
  #############################################
  # TAKES: N; Number of simulations
  #        n, number of pairs in the simulation
  #        theta; vector of 3 thetas comparing
  #        powern1; 3 samples sizes for each theta given a certain estimate size
  #        powern2; 3 samples sizes for each theta given a certain estimate size 
  #        powern3; 3 samples sizes for each theta given a certain estimate size 
  #        rowNames; Name of each row of the table which should be a theta value
  #        colNames; Name of each column of the table which should be power comparisons
  # RETURNS: 3x3 table
  #############################################
  
  tab<-matrix(data = NA, nrow = 3, ncol = 3, dimnames = list(rowNames, colNames))
  # keeps the same theta but with different powers for that theta size
  sim1<- ttestsim(N, n[1], theta[1], theta[1], theta[1], powern1[1], powern2[1], powern3[1])
  tab[1,]<-apply(sim1, 2, FUN = function(x) sum(abs(x) > qt(0.975, df =n-1))/N)

  # keeps the same theta but with different powers for that theta size
  sim2<- ttestsim(N, n[2], theta[2], theta[2], theta[2], powern1[2], powern2[2], powern3[2])
  tab[2,]<-apply(sim2, 2, FUN = function(x) sum(abs(x) > qt(0.975, df =n-1))/N)
  
  # keeps the same theta but with different powers for that theta size
  sim3<- ttestsim(N, n[3], theta[3], theta[3], theta[3], powern1[3], powern2[3], powern3[3])
  tab[3,]<-apply(sim3, 2, FUN = function(x) sum(abs(x) > qt(0.975, df =n-1))/N) 
  
  return(tab)
}
##################
#######################
###################################

N<-1000
n<-c(100, 100, 100)
theta<-c(0.2,0.5,0.8)
power40<-c(145.6332,23.30131,9.102075)
power60<-c(244.9453,39.19124,15.309079)
power80<-c(392.4541,62.79265,24.52838)
row<-c("Theta 0.2", "Theta 0.5", "Theta 0.8")
col<-c("Power 40/60", "Power 40/80", "Power 60/80")
# c2table<-case2tab(N, n,theta, power80,power60, power40, row, col)
# takes a bootstrap of size 100 for case2
boot2<-replicate(100, case2tab(N, n,theta, power80,power60, power40, row, col))
# empty mean and sd tables
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




#####################################################################################
######################################################################################
# analysis with different n's
# with theta 0.8 and case 2 scenario
# n's are 100, 50, 25
N<-1000
n<-c(100,50,25)
theta<-c(0.8,0.8,0.8)
power40<-c(9.102075, 9.102075, 9.102075)
power60<-c(15.19124,15.19124, 15.19124)
power80<-c(24.52838, 24.52838, 24.52838)
row<-c("n 100", "n 50", "n 25")
col<-c("Power 40/60", "Power 40/80", "Power 60/80")
#c2table<-case2tab(N, n, theta, power80,power60, power40, row, col)

bootmultn<-replicate(100, case2tab(N,n,theta, power40,power60, power80, row, col))

multinmeans<-matrix(data = NA, nrow=3, ncol= 3, dimnames = list(c("n 100", " n 50", "n 25"),c("Power 40/60", "Power 40/80", "Power 60/80")))
multinsd<-matrix(data = NA, nrow=3, ncol= 3, dimnames = list(c("n 100", " n 50", "n 25"),c("Power 40/60", "Power 40/80", "Power 60/80")))

multinmeans[1,1]<-mean(bootmultn[1, 1,])
multinsd[1,1]<-sd(bootmultn[1, 1,])
multinmeans[1,2]<-mean(bootmultn[1,2,])
multinsd[1,2]<-sd(bootmultn[1, 2,])
multinmeans[1,3]<-mean(bootmultn[1,3,])
multinsd[1,3]<-sd(bootmultn[1, 3,])

multinmeans[2,1]<-mean(bootmultn[2, 1,])
multinsd[2,1]<-sd(bootmultn[2, 1,])
multinmeans[2,2]<-mean(bootmultn[2,2,])
multinsd[2,2]<-sd(bootmultn[2, 2,])
multinmeans[2,3]<-mean(bootmultn[2,3,])
multinsd[2,3]<-sd(bootmultn[2, 3,])

multinmeans[3,1]<-mean(bootmultn[3, 1,])
multinsd[3,1]<-sd(bootmultn[3, 1,])
multinmeans[3,2]<-mean(bootmultn[3,2,])
multinsd[3,2]<-sd(bootmultn[3, 2,])
multinmeans[3,3]<-mean(bootmultn[3,3,])
multinsd[3,3]<-sd(bootmultn[3, 3,])

multinmeans
multinsd

