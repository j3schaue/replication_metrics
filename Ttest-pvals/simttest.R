#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("./src/metrics_funs.R")
### Case 1 different thetas but same power
N<- 1000
tmatrix<-matrix(data = NA, nrow = N, ncol = 3)
for (k in 1:N){
  
  t1<-rep(.2, 100)
  sim_1_small<-simulate_studies(t1, 4/393) # 393 sample size needed for 80% power is small 
  t2<-rep(.5, 100)
  sim_1_med<-simulate_studies(t2, 4/64) # 64 is samples size needed for 80% power if medium
  t3<-rep(.8,100)
  sim_1_large<-simulate_studies(t3, 4/25) # 25 is samples size needed for 80% power if medium
  
  #difference in each pvalue
  diff_smallmed<-sim_1_small$p - sim_1_med$p 
  diff_smalllarge<-sim_1_small$p - sim_1_large$p 
  diff_medlarge<-sim_1_med$p - sim_1_large$p 
  
  # find t statistics
  tsmallmed<- mean(diff_smallmed)/(sd(diff_smallmed)/10)
  tsmalllarge<- mean(diff_smalllarge)/(sd(diff_smalllarge)/10)
  tmedlarge<- mean(diff_medlarge)/(sd(diff_medlarge)/10)
  
  tmatrix[k,1]<-tsmallmed
  tmatrix[k,2]<-tsmalllarge
  tmatrix[k,3]<-tmedlarge
  
}
case1_smallmed<-tmatrix[,1]
case1_smalllarge<-tmatrix[,2]
case1_medlarge<-tmatrix[,3]
sum(tmatrix[,1] > 1.98)/N
sum(tmatrix[,2] > 1.98)/N
sum(tmatrix[,3] > 1.98)/N

  