power_sims<-function(N,M,delta,n){
  ###############################################################################
  # TAKES: N; number of simulations 
  #        M; vector of number of false negatives
  #        delta; effect size under alternative hypothesis, on scale of cohen's d
  #        n; median total sample size across studies
  # RETURNS: power of Fisher's test to reject
  # Assumes 2-sided p-values, throws away p-values<=0.05 to match OSC methods
  ###############################################################################
  T<-c()
  Power<-matrix()
  for(k in 1:length(M)){
    print(paste("m=",M[k]))
    for (i in 1:N){
      p0<-runif(64-M[k],0.05,1) #draws p-values for the true negatives
      p1<-c() 
      for (j in 1:M[k]){
        p1[j]<-0
        while (p1[j]<=0.05) { #throw away p-values<=0.05
          p1[j]<-2*(1-pnorm(abs(rnorm(1,delta,sqrt(2/n)))/sqrt(2/n))) #calculate p-values for the false negatives
        }
      }
      #test statistic for Fisher's method, with transformation for truncating p-values
      T[i]<--2*sum(log((p0-0.05)/0.95))-2*sum(log((p1-0.05)/0.95)) 
    }
    Power[k]<-sum(T>155.4047)/N
  }
  return(Power)
}

M<-c(1,2,3,4,5,10,32,64)
n<-38
power_0.2<-power_sims(100000,M,0.2,n)
power_0.5<-power_sims(100000,M,0.5,n)
power_0.8<-power_sims(100000,M,0.8,n)

power_sims_table<-data.frame(M,power_0.2,power_0.5,power_0.8)
saveRDS(power_sims_table,"Fisher_power_sims.Rds")
