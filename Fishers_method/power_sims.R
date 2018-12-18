power_sims<-function(N,M,delta,n,k){
  #########################################################################################
  # TAKES: N; number of simulations 
  #        M; vector of number of non-null effects
  #        delta; effect size under alternative hypothesis, on scale of cohen's d
  #        n; vector of treatment/control sample size across studies (total sample size/2)
  #        k; number of studies
  # RETURNS: 5 column dataframe of results [Power N k M delta]
  # Assumes 2-sided p-values, throws away p-values<=0.05 to match OSC methods
  #########################################################################################
  
  T<-c() #empty list to store Fisher's test statistic 
  power<-matrix() #empty matrix to store results
 
  for(l in 1:length(M)){
    
    for (i in 1:N){
      
      #print(i) # can uncomment to show progress for lengthy simulations
      
      p0<-runif(k - M[l], 0.05, 1) #draws p-values for the true null effects
      
      p1<-c() #create list to store p-values drawn for non-null effects
      z<-c()
      for (j in 1:M[l]){ 
        
        p1[j]<-0
        
        while (p1[j] <= 0.05) { #throw away p-values<=0.05
          z[j] <- rnorm(1,delta, sqrt(2/n[j] + delta^2/(4*n[j])))
          p1[j] <- 2*(1-pnorm(abs(z[j])/sqrt(2/n[j] + delta^2/(4*n[j]))))
        }
        
        #print(n[j]) # can uncomment to show progress for lengthy simulations 
      }
      
      #test statistic for Fisher's method, with transformation for truncating p-values
      T[i]<--2*sum(log((p0-0.05)/0.95))-2*sum(log((p1-0.05)/0.95)) 
      
    }
    
    power[l]<-sum(T > qchisq(0.95, 2*k) )/N
    print(power[l])
    if(power[l]>0.99) break
  }
  
  data <- as.data.frame(cbind(power = power, n = n[1:length(power)], k = rep(k,length(power)), M = M[1:length(power)], delta = rep(delta,length(power))))
  return(data)
}

M<-c(1,2,3) #vector of number of non-null effects
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

source("./src/metrics_funs.R")
replicate_power<-data.frame(n,power_delta_0.2=power_fun(0.2,2/n),power_delta_0.5=power_fun(0.5,2/n),power_delta_0.8=power_fun(0.8,2/n))
#set largest to be the largest sample size among the studies for which there was at most 99.99% power for a given delta
n_delta_0.2<-c(745,n[2:64]) #largest study had >99.99% power to detect delta=0.2
n_delta_0.5<-c(rep(159,3),n[4:64])#3 largest studies had >99.99% power to detect delta=0.5
n_delta_0.8<-c(rep(100,11),n[12:64]) #11 largest studies had >99.99% power to detect delta=0.8
delta_0.2_large_n<-power_sims(100000,M,0.2,n_delta_0.2) #starts at n=745
delta_0.5_large_n<-power_sims(100000,M,0.5,n_delta_0.5) #starts at n=159
delta_0.8_large_n<-power_sims(100000,M,0.8,n_delta_0.8) #starts at n=100

large_n_used<-data.frame(n_delta_0.2,n_delta_0.5,n_delta_0.8)
power_large_n<-data.frame(M,delta_0.2_large_n,delta_0.5_large_n,delta_0.8_large_n)
saveRDS(power_large_n,"power_large_n.RDS")
saveRDS(large_n_used,"large_n_used.RDS")

###CREATE PLOT####
k10 <- readRDS("./k10_data.RDS")
k50 <- readRDS("./k50_data.RDS")
k100 <- readRDS("./k100_data.RDS")

library(ggplot2)
n_labels <- c('25' = "n=25", '50' = "n=50", '75' = "n=75", '100'= "n=100")
k_labels <- c( '10' = "k=10", '50' = "k=50", '100' = "k=100")

k10$prop <- k10$M/k10$k
k50$prop <- k50$M/k50$k
k100$prop <- k100$M/k100$k

all <- rbind(k10,k50,k100)
plot_all <- ggplot(all, aes(x = prop, y = power, color = factor(delta))) +
  facet_grid(k~n, labeller = labeller(n = n_labels, k = k_labels)) +
  theme(panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer(palette="Dark2")+
  scale_x_continuous(name = "Proportion of false negatives", limits = c(0,1), breaks = seq(0,1,.10)) +
  scale_y_continuous(name = "Power", limits = c(0,1), breaks = seq(0,1,0.1))+
  #geom_point(size = 3) +
  #facet_grid(.~n, labeller = labeller(n = labels)) +
  labs(title = "Power of Fisher's method", color = expression(delta))+
  #geom_smooth(method = "loess", se = FALSE, size = 0.5)
  geom_line()



diane<-function(delta,n){
  p1<-c()
  z <- c()
  for (j in 1:100000){ 
    #p1[j]<-2*(1 - pnorm(abs(rnorm(1, delta, sqrt(2/n + delta^2 / (4*n))))/sqrt(2/n + delta^2 / (4*n))))
    #p1[j]<-2*(1 - pnorm(abs(rnorm(1, 1/2*2*n*1/4*delta^2, sqrt(2*n*1/4*delta^2)))/sqrt(2*n*1/4*delta^2)))
    z[j] <- rnorm(1,delta, sqrt(2/n + delta^2/(4*n)))
    #p1[j] <- (1-pnorm(z[j]/sqrt(2/n + delta^2/(4*n))))
    p1[j] <- 2*(1-pnorm(abs(z[j])/sqrt(2/n + delta^2/(4*n)))) #two-sided
  }
  D<--2*log(p1)
}


plot <- function(delta, n){
  d1 <- as.data.frame(diane(delta,n))
  colnames(d1)[1]<-"V1"
  ggplot(d1, aes(x = V1)) +
  scale_x_continuous(name = "-2log(p_i)", limits = c(-20,80), breaks = seq(-20,100,10)) +
  geom_histogram(aes(y = ..density..)) +
  #stat_function(fun = dnorm, args = list(mean = 2*n*1/4*delta^2, sd = sqrt(2*n*delta^2))) +
  stat_function(fun = dnorm, args = list(mean = 1/2*2*n*1/4*delta^2, sd = sqrt(2*n*1/4*delta^2))) + #two-sided
  labs(title = n)
}

p1 <- plot(0.2,700)
p2 <- plot(0.2,1500)
p3 <- plot(0.5,100)
p4 <- plot(0.5,250)
p5 <- plot(0.8,50)
p6 <- plot(0.8,100)
grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 3, ncol = 2)

library(latex2exp)
par(mfrow=c(3,2))

hist(dist_of_X2(0.2,700), main=TeX('n=700, $\\theta$=0.2'), xlab=expression(-2*log(p_i)))
x<-seq(0,60,0.1)
curve(dnorm(x, mean = 0.5*700*2*1/4*0.2^2, sd = sqrt(2*700*1/4*0.2^2)), add = TRUE)
hist(dist_of_X2(0.2,1500), main=TeX('n=1500, $\\theta$=0.2'), xlab=expression(-2*log(p_i)))

hist(dist_of_X2(0.5,100), main=TeX('n=100, $\\theta$=0.5'),  xlab=expression(-2*log(p_i)))
hist(dist_of_X2(0.5,250), main=TeX('n=250, $\\theta$=0.5'),  xlab=expression(-2*log(p_i)))

hist(dist_of_X2(0.8,50), main=TeX('n=50, $\\theta$=0.8'),  xlab=expression(-2*log(p_i)))
hist(dist_of_X2(0.8,100), main=TeX('n=100, $\\theta$=0.8'),  xlab=expression(-2*log(p_i)))
  