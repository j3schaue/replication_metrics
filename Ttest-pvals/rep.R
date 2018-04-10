# thetas are different
## theta1=0.8 theta2=-0.8 with same n

# N is the number of times repeated
N<-10000
# both n1 and n2 have the same sample size of 100 
# since their estimates are the same size but in opposite directions

n1<-100
n2<-100
ts<-c()
for (k in 1:N){
  # creates two vectors of estimates from original to replicate
  # pulling 100 random draws still not sure how many to pull?????
  t1<- rnorm(100, 0.8,sqrt(4/n1)) #not sure about the variance ?????
  t2<-rnorm(100,-0.8, sqrt(4/n2))
      # create the z score and then pvalues for each of the estimates
      # still questioning the variance of this? 
      z1<-t1/sqrt(4/n1)
      p1<-2*pnorm(-abs(z1))
      z2<-t2/sqrt(4/n2)
      p2<-2*pnorm(-abs(z2))
  # take the difference of the two groups of pvalues
  diff<-p1-p2
  # get the t stat for the pair t test
  t<-mean(diff)/(sd(diff)/10)
  # store in vector
  ts[k]<-t
}

hist(ts)
# 2.64 is what was written down in notes but is it 1.98???
sum(abs(ts) > 1.98) # number of t stats that are over the critical value
sum(abs(ts) > 1.98)/N # the probability that the test would conclude that the means are different



## B where is theta1 is 0.8 and theta2 is 0.2
## 16n1=n2 since they have different effect sizes small and large
n1<- 52
n2<-2*393
ts<-c()
for (k in 1:N){
  t1<- rnorm(100, 0.8,sqrt(4/n1))
  t2<-rnorm(100,0.2, sqrt(4/n2))
 
  z1<-t1/sqrt(4/n1)
  p1<-2*pnorm(-abs(z1))
  z2<-t2/sqrt(4/n2)
  p2<-2*pnorm(-abs(z2))
  
  diff<-p1-p2
  t<-mean(diff)/(sd(diff)/10)
  ts[k]<-t
}
max(ts)
sum(ts > 1.98)
sum(ts > 1.98)/N
hist(ts)



### when thetas are the same but test stat is different
## might be right idk \_(%)_/ ??????
# different sample sizes but with the same estimate size.
n1<- 100
n2<-10*n1
ts<-c()
for (k in 1:N){
  t1<- rnorm(100, 0.8,sqrt(4/n1))
  t2<-rnorm(100,0.8, sqrt(4/n2))
  
  z1<-t1/sqrt(4/n1)
  p1<-2*pnorm(-abs(z1))
  z2<-t2/sqrt(4/n2)
  p2<-2*pnorm(-abs(z2))
  
  diff<-p1-p2
  t<-mean(diff)/(sd(diff)/10)
  ts[k]<-t
}
max(ts)
sum(ts > 2.64)
sum(ts > 1.98)/N
hist(ts)

