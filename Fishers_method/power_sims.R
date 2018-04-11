power_sims<-function(N,m,delta,n){
  ###############################################################################
  # TAKES: N; number of simulations 
  #        m; number of false negatives
  #        delta; effect size under alternative hypothesis, on scale of cohen's d
  #        n; median total sample size across studies
  # RETURNS: power of Fisher's test to reject
  # Assumes 2-sided p-values, throws away p-values<=0.05 to match OSC methods
  ###############################################################################
  T<-c()
  for (i in 1:N){
    p0<-runif(64-m,0.05,1) #draws p-values for the true negatives
    p1<-c() 
    for (j in 1:m){
      p1[j]<-0
      while (p1[j]<=0.05) { #throw away p-values<=0.05
        p1[j]<-2*(1-pnorm(abs(rnorm(1,delta,sqrt(4/n))))) #calculate p-values for the false negatives
      }
    }
    #test statistic for Fisher's method, with transformation for truncating p-values
    T[i]<--2*sum(log((p0-0.05)/0.95))-2*sum(log((p1-0.05)/0.95)) 
  }
  power<-sum(T>155.4047)/N
  print(power)
}
n<-76
delta<-0.5
power_sims(100000,1,delta,n)
power_sims(100000,2,delta,n)
power_sims(100000,3,delta,n)
power_sims(100000,4,delta,n)
power_sims(100000,5,delta,n)
power_sims(100000,10,delta,n)
power_sims(100000,32,delta,n)
power_sims(100000,64,delta,n)