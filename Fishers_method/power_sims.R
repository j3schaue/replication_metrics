power_sims<-function(N,M,delta,n){
  #########################################################################################
  # TAKES: N; number of simulations 
  #        M; vector of number of non-null effects
  #        delta; effect size under alternative hypothesis, on scale of cohen's d
  #        n; vector of treatment/control sample size across studies (total sample size/2)
  # RETURNS: power of Fisher's test to reject
  # Assumes 2-sided p-values, throws away p-values<=0.05 to match OSC methods
  #########################################################################################
  T<-c() #empty list for Fisher's test statistic 
  Power<-matrix() #empty matrix to store results
  for(k in 1:length(M)){
    for (i in 1:N){
      #print(i) #uncomment to show progress for lengthy simulations
      p0<-runif(64-M[k],0.05,1) #draws p-values for the true negatives
      p1<-c() 
      for (j in 1:M[k]){
        p1[j]<-0
        while (p1[j]<=0.05) { #throw away p-values<=0.05
          p1[j]<-2*(1-pnorm(abs(rnorm(1,delta,sqrt(2/n[j])))/sqrt(2/n[j])))
        }
        #print(n[j]) #uncomment to show progress for lengthy simulations 
      }
      #test statistic for Fisher's method, with transformation for truncating p-values
      T[i]<--2*sum(log((p0-0.05)/0.95))-2*sum(log((p1-0.05)/0.95)) 
    }
    Power[k]<-sum(T>155.4047)/N
  }
  return(Power)
}

M<-c(1,2,3,4,5,10,32,64) #vector of number of non-null effects
n<-38 #median sample size (76) divided by 2 
power_delta_0.2<-power_sims(100000,M,0.2,n)
power_delta_0.5<-power_sims(100000,M,0.5,n)
power_delta_0.8<-power_sims(100000,M,0.8,n)

power_sims_table<-data.frame(M,power_0.2,power_0.5,power_0.8)
saveRDS(power_sims_table,"Fisher_power_sims.Rds")

#test upper bounds of power (by largest sample size study having false negatives)
rpp_data<-read.csv("~/Desktop/NU/Replication/replication_metrics/data/rpp_data.csv")
data_forFisher<- rpp_data[ !is.na(rpp_data$T_pval_USE..R.) & (rpp_data$T_pval_USE..R.>0.05)  , ]
n<-sort(data_forFisher$N..R.,decreasing=TRUE)/2

replicate_power<-data.frame(n,power_delta_0.2=power_fun(0.2,2/n),power_delta_0.5=power_fun(0.5,2/n),power_delta_0.8=power_fun(0.8,2/n))
delta_0.2_n_745<-power_sims(100000,1,0.2,745) #100% power to detect originally with largest n=384351.5, so move to n=745: power_fisher=0.08206
delta_0.5_n_159<-power_sims(100000,1,0.5,159) #100% power to detect originally with n=573, moved to n=159: power_fisher=0.08676
delta_0.8_large_n<-power_sims(100000,M,0.8,n) #start at n=100 power_fisher=0.09329
delta_0.2_large_n<-power_sims(100000,M,0.2,n) #start at n=745 power_fisher=0.0
delta_0.5_large_n<-power_sims(100000,M,0.5,n) #start at n=573 power_fisher=0.0

