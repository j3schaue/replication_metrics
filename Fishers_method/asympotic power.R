power_median<-function(n,delta){
  1-pnorm(155.4047, mean=64*n*(1/4)*(delta^2), sqrt(64*4*n*(1/4)*(delta^2)))
}
par(mfrow=c(3,1))
plot(power_median(1:1000,0.2), type='l')
plot(power_median(1:1000,0.5), type='l')
plot(power_median(1:1000,0.8), type='l')

s<-c()
Power<-c()
power<-function(n,delta,M){
  for(j in 1:length(M)){
    for(i in 1:M[j]){
      s[i]<-n[i]*1/4*delta^2
    }
    Power[j]<-1-pnorm(155.4047,1/2*sum(s),sqrt(sum(s)))+pnorm(-155.4047,1/2*sum(s),sqrt(sum(s)))
  }
  return(Power)
}
n<-rep(76,64)
M<-c(1,2,3,4,5,10,32,64) #vector of number of non-null effects
power(n,0.2)
power(n,0.5)
power(n,0.8)
x1<-power(large_n_used[,1],0.2,M)
x2<-power(large_n_used[,2],0.5,M)
x3<-power(large_n_used[,3],0.8,M)
