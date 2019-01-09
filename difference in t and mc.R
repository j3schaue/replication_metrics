##-----------------------------------------------##
## Sourced Scripts
##-----------------------------------------------##
source("./src/metrics_funs.R")

##-----------------------------------------------##
## McNemar's Test Function
##-----------------------------------------------##

comb_fun <- function(theta1, theta2, n1, n2, k){
  
  #############################################
  # TAKES: theta1; a vector of means (original)
  #        theta2; a vector of means (replication)
  #        n1; sample size (original)
  #        n2; sample size (replication)
  #        k; number of study pairs
  #
  # RETURNS: p-value of McNemar's Test & T-test
  #############################################
  hold<-matrix(data = NA, nrow = 1, ncol = 2)
  
  orig.studies <- simulate_studies(rep(theta1, k), 2/n1) # simulate k original studies
  rep.studies  <- simulate_studies(rep(theta2, k), 2/n2) # simulate k replication studies
  
  # number of study pairs for which original finding is significant and replication finding is not
  b <- length(which(orig.studies$p <=.05 & rep.studies$p > .05)) 
  
  # number of study pairs for which original finding is not significant and replication finding is
  c <- length(which(orig.studies$p > .05 & rep.studies$p <= .05))
  
  mc.stat <- ((b-c)^2)/(b+c) # calculate the McNemar's test statistic
  
  hold[1,1]<-pchisq(mc.stat, 1, lower.tail=F) # obtain the p-value for McNemar's test in 
  
  diff<-orig.studies$p - rep.studies$p 
  t<- mean(diff)/(sd(diff)/sqrt(k))
  hold[1,2]<-2*pt(-abs(t), k-1)

  return(hold)  
}
###################################################################
N<-1000
##################################################
################ type 2 ##############################

# 80%
sl80 <- replicate(N, comb_fun(0.2, 0.8, 392.4541, 24.52838, 100))
sl80<-cbind(sl80[1,1,], sl80[1,2,]) 
sig.mc.sl80<-sum(sl80[,1] <= 0.05)
sig.t.sl80<-sum(sl80[,2] <= 0.05)
difsig.sl80<-abs(sig.mc.sl80 - sig.t.sl80)

sm80 <- replicate(N, comb_fun(0.2, 0.5, 392.4541, 62.79265, 100))
sm80<-cbind(sm80[1,1,], sm80[1,2,])
sig.mc.sm80<-sum(sm80[,1] <= 0.05)
sig.t.sm80<-sum(sm80[,2] <= 0.05)
difsig.sm80<-abs(sig.mc.sm80 - sig.t.sm80)

ml80 <- replicate(N,comb_fun(0.5, 0.8, 62.79265, 24.52838, 100))
ml80<-cbind(ml80[1,1,], ml80[1,2,])
sig.mc.ml80<-sum(ml80[,1] <= 0.05)
sig.t.ml80<-sum(ml80[,2] <= 0.05)
difsig.ml80<-abs(sig.mc.ml80 - sig.t.ml80)

#60%
sl60 <- replicate(N, comb_fun(0.2, 0.8, 244.9453, 15.309079, 100))
sl60<-cbind(sl60[1,1,], sl60[1,2,]) 
sig.mc.sl60<-sum(sl60[,1] <= 0.05)
sig.t.sl60<-sum(sl60[,2] <= 0.05)
difsig.sl60<-abs(sig.mc.sl60 - sig.t.sl60)

sm60 <- replicate(N, comb_fun(0.2, 0.5, 244.9453, 39.19124, 100))
sm60<-cbind(sm60[1,1,], sm60[1,2,])
sig.mc.sm60<-sum(sm60[,1] <= 0.05)
sig.t.sm60<-sum(sm60[,2] <= 0.05)
difsig.sm60<-abs(sig.mc.sm60 - sig.t.sm60)

ml60 <- replicate(N, comb_fun(0.5, 0.8, 39.19124, 15.309079, 100))
ml60<-cbind(ml60[1,1,], ml60[1,2,])
sig.mc.ml60<-sum(ml60[,1] <= 0.05)
sig.t.ml60<-sum(ml60[,2] <= 0.05)
difsig.ml60<-abs(sig.mc.ml60 - sig.t.ml60)

#40%
sl40 <- replicate(N, comb_fun(0.2, 0.8, 145.6332, 9.102075, 100))
sl40<-cbind(sl40[1,1,], sl40[1,2,]) 
sig.mc.sl40<-sum(sl40[,1] <= 0.05)
sig.t.sl40<-sum(sl40[,2] <= 0.05)
difsig.sl40<-abs(sig.mc.sl40 - sig.t.sl40)

sm40 <- replicate(N, comb_fun(0.2, 0.5, 145.6332, 23.30131, 100))
sm40<-cbind(sm40[1,1,], sm40[1,2,])
sig.mc.sm40<-sum(sm40[,1] <= 0.05)
sig.t.sm40<-sum(sm40[,2] <= 0.05)
difsig.sm40<-abs(sig.mc.sm40 - sig.t.sm40)

ml40 <- replicate(N, comb_fun(0.5, 0.8, 23.30131, 9.102075, 100))
ml40<-cbind(ml40[1,1,], ml40[1,2,])
sig.mc.ml40<-sum(ml40[,1] <= 0.05)
sig.t.ml40<-sum(ml40[,2] <= 0.05)
difsig.ml40<-abs(sig.mc.ml40 - sig.t.ml40)

diff40<-c(difsig.sl40, difsig.sm40, difsig.ml40)
diff60<-c(difsig.sl60, difsig.sm60, difsig.ml60)
diff80<-c(difsig.sl80, difsig.sm80, difsig.ml80)
typetwo<-rbind(diff40, diff60, diff80)
colnames(typetwo)<-c("0.2/0.8", "0.2/0.5", "0.5/0.8")
######### Absolute value of difference of significant results between the two tests of 10000 runs
typetwo/N

#####################################################
####################### type 1 ###########################

## 40%/80%

lh2 <- replicate(N, comb_fun(0.2, 0.2, 145.6332, 392.4541, 100))
lh2<-cbind(lh2[1,1,], lh2[1,2,])
#mclh2<-sum(lh2[,1] >= 0.05)
#tlh2<-sum(lh2[,2] >= 0.05)
#d<-abs(mclh2-tlh2)

dif.lh2<-abs(sum(lh2[,1] >= 0.05) - sum(lh2[,2] >= 0.05))

lh5 <- replicate(N, comb_fun(0.5, 0.5, 23.30131, 62.79265, 100))
lh5<-cbind(lh5[1,1,], lh5[1,2,])
dif.lh5<-abs(sum(lh5[,1] >= 0.05) - sum(lh5[,2] >= 0.05))

lh8 <- replicate(N, comb_fun(0.8, 0.8, 9.102075, 24.52838, 100))
lh8<-cbind(lh8[1,1,], lh8[1,2,])
dif.lh8<-abs(sum(lh8[,1] >= 0.05) - sum(lh8[,2] >= 0.05))


## 60%/80%
mh2 <- replicate(N, comb_fun(0.2, 0.2, 244.9453, 392.4541, 100))
mh2<-cbind(mh2[1,1,], mh2[1,2,])
dif.mh2<-abs(sum(mh2[,1] >= 0.05) - sum(mh2[,2] >= 0.05))

mh5 <- replicate(N, comb_fun(0.5, 0.5, 39.19124, 62.79265, 100))
mh5<-cbind(mh5[1,1,], mh5[1,2,])
dif.mh5<-abs(sum(mh5[,1] >= 0.05) - sum(mh5[,2] >= 0.05))

mh8 <- replicate(N, comb_fun(0.8, 0.8, 15.309079, 24.52838, 100))
mh8<-cbind(mh8[1,1,], mh8[1,2,])
dif.mh8<-abs(sum(mh8[,1] >= 0.05) - sum(mh8[,2] >= 0.05))

## 40%/ 60%
lm2 <- replicate(N, comb_fun(0.2, 0.2, 145.6332, 244.9453, 100))
lm2<-cbind(lm2[1,1,], lm2[1,2,])
dif.lm2<-abs(sum(lm2[,1] >= 0.05) - sum(lm2[,2] >= 0.05))

lm5 <- replicate(N, comb_fun(0.5, 0.5, 23.30131, 39.19124, 100))
lm5<-cbind(lm5[1,1,], lm5[1,2,])
dif.lm5<-abs(sum(lm5[,1] >= 0.05) - sum(lm5[,2] >= 0.05))

lm8 <- replicate(N, comb_fun(0.8, 0.8, 9.102075, 15.309079, 100))
lm8<-cbind(lm8[1,1,], lm8[1,2,])
dif.lm8<-abs(sum(lm8[,1] >= 0.05) - sum(lm8[,2] >= 0.05))

fourtysixty<-c(dif.lm2, dif.lm5, dif.lm8)
sixtyeighty<-c(dif.mh2, dif.mh5, dif.mh8)
fourtyeighty<-c(dif.lh2, dif.lh5, dif.lh8)

typeone<-cbind(fourtysixty, sixtyeighty, fourtyeighty)
rownames(typeone)<- c("0.2", "0.5", "0.8")
### number of differences between mcnemars and t test for type 1 situation
typeone/N
