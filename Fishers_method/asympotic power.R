power<-function(n,delta){
  1-pnorm(155.4047, mean=64*n*(1/4)*(delta^2), sqrt(64*4*n*(1/4)*(delta^2)))
}
par(mfrow=c(3,1))
plot(power(1:1000,0.2), type='l')
plot(power(1:1000,0.5), type='l')
plot(power(1:1000,0.8), type='l')
