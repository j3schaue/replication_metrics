power_sims<-function(N, M, delta, n, k){
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
  
  data <- as.data.frame(cbind(power = power, k = rep(k,length(power)), M = M[1:length(power)], delta = rep(delta,length(power))))
  return(data)
}

########################################
### RPP DATA SIMULATIONS             ###
########################################

library(tidyverse)

#test upper bounds of power (by largest sample size study having false negatives)
rpp_data_cleaned <- read_csv("Fishers_method/rpp_data_cleaned.csv")
rpp_fishers <- subset(rpp_data_cleaned, rpp_data_cleaned$replicate == 1 & rpp_data_cleaned$pvalr > 0.05)

#compute effectve sample size based on effect size and variance
rpp_fishers$n_effective <- (8+(rpp_fishers$d)^2)/(4*rpp_fishers$vd)
rpp_fishers$n_effective <- ifelse(rpp_fishers$experiment == "Koo", max(rpp_fishers$n_effective,na.rm =TRUE), rpp_fishers$n_effective)
n<-sort(rpp_fishers$n_effective,decreasing=TRUE, na.last = TRUE)


source("./src/metrics_funs.R")
replicate_power<-data.frame(n,power_delta_0.2=power_fun(0.2,2/n + 0.2^2/(4*n)),power_delta_0.5=power_fun(0.5,2/n + 0.5^2/(4*n)),power_delta_0.8=power_fun(0.8,2/n + 0.8^2/(4*n)))
#set largest to be the largest sample size among the studies for which there was at most 99.99% power for a given delta
n_delta_0.2<- n #all studies had <99.99% power to detect delta=0.2
n_delta_0.5<-c(rep(replicate_power$n[4],3),n[4:64])#3 largest studies had >99.99% power to detect delta=0.5
n_delta_0.8<-c(rep(replicate_power$n[12],11),n[12:64]) #11 largest studies had >99.99% power to detect delta=0.8

M <- c(1,2,3,4,5,round(64*seq(0.1,1,0.1))) #want simulations for 1-5 studies and at each 10th percentile
delta_0.2_large_n<-power_sims(100000, M, 0.2, n_delta_0.2, 64) #starts at n=745
delta_0.5_large_n<-power_sims(10000, M, 0.5, n_delta_0.5, 64) #starts at n=159
delta_0.8_large_n<-power_sims(100000, M, 0.8, n_delta_0.8, 64) #starts at n=100

rpp_power_results <- rbind(delta_0.2_large_n, delta_0.5_large_n, delta_0.8_large_n)
saveRDS(rpp_power_results ,"rpp_power_results.RDS")

n_worst <- sort(rpp_fishers$n_effective,decreasing=FALSE, na.last = TRUE)
source("./src/metrics_funs.R")
replicate_power<-data.frame(n_worst,power_delta_0.2=power_fun(0.2,2/n_worst + 0.2^2/(4*n_worst)),power_delta_0.5=power_fun(0.5,2/n_worst + 0.5^2/(4*n_worst)),power_delta_0.8=power_fun(0.8,2/n_worst + 0.8^2/(4*n_worst)))
#set largest to be the largest sample size among the studies for which there was at most 99.99% power for a given delta
n_worst_delta_0.2<- n_worst #all studies had <99.99% power to detect delta=0.2
n_worst_delta_0.5<-c(n_worst[1:61], rep(replicate_power$n_worst[61],3))#3 largest studies had >99.99% power to detect delta=0.5
n_worst_delta_0.8<-c(n_worst[1:53], rep(replicate_power$n_worst[53],11)) #11 largest studies had >99.99% power to detect delta=0.8

M <- c(1,2,3,4,5,round(64*seq(0.1,1,0.1))) #want simulations for 1-5 studies and at each 10th percentile
delta_0.2_large_n_worst<-power_sims(10000, M, 0.2, n_worst_delta_0.2, 64) #starts at n_worst=745
delta_0.5_large_n_worst<-power_sims(10000, M, 0.5, n_worst_delta_0.5, 64) #starts at n_worst=159
delta_0.8_large_n_worst<-power_sims(10000, M, 0.8, n_worst_delta_0.8, 64) #starts at n_worst=100

rpp_power_results_worst <- rbind(delta_0.2_large_n_worst, delta_0.5_large_n_worst, delta_0.8_large_n_worst)
saveRDS(rpp_power_results_worst ,"rpp_power_results_worst.RDS")


########################################
### RPE DATA SIMULATIONS             ###
########################################

full_data_rep_fisher <- readRDS("Fishers_method/full_data_rep_fisher.RDS")
rpe_fishers <- full_data_rep_fisher[full_data_rep_fisher$data == "RPE" & full_data_rep_fisher$site == "replicate" & full_data_rep_fisher$replicated == 0,]

rpe_fishers$n_effective <- (8+(rpe_fishers$d)^2)/(4*rpe_fishers$vd)
n<-sort(rpe_fishers$n_effective,decreasing=TRUE, na.last = TRUE)

source("./src/metrics_funs.R")
replicate_power<-data.frame(n,power_delta_0.2=power_fun(0.2,2/n + 0.2^2/(4*n)),power_delta_0.5=power_fun(0.5,2/n + 0.5^2/(4*n)),power_delta_0.8=power_fun(0.8,2/n + 0.8^2/(4*n)))
#set largest to be the largest sample size among the studies for which there was at most 99.99% power for a given delta
n_delta_0.2<- n #all studies had <99.99% power to detect delta=0.2
n_delta_0.5<-n #all studies had <99.99% power to detect delta=0.5
n_delta_0.8<-c(rep(replicate_power$n[3],2),n[3:7]) #11 largest studies had >99.99% power to detect delta=0.8

M <- c(1,2,3,4,5,6,7)
delta_0.2_large_n<-power_sims(100000, M, 0.2, n_delta_0.2, 7) #starts at n=745
delta_0.5_large_n<-power_sims(100000, M, 0.5, n_delta_0.5, 7) #starts at n=159
delta_0.8_large_n<-power_sims(100000, M, 0.8, n_delta_0.8, 7) #starts at n=100

rpe_power_results <- rbind(delta_0.2_large_n, delta_0.5_large_n, delta_0.8_large_n)
saveRDS(rpe_power_results ,"rpe_power_results.RDS")

n_worst <-sort(rpe_fishers$n_effective,decreasing=FALSE, na.last = TRUE)
replicate_power<-data.frame(n_worst,power_delta_0.2=power_fun(0.2,2/n_worst + 0.2^2/(4*n_worst)),power_delta_0.5=power_fun(0.5,2/n_worst + 0.5^2/(4*n_worst)),power_delta_0.8=power_fun(0.8,2/n_worst + 0.8^2/(4*n_worst)))
#set largest to be the largest sample size among the studies for which there was at most 99.99% power for a given delta
n_worst_delta_0.2<- n_worst #all studies had <99.99% power to detect delta=0.2
n_worst_delta_0.5<- n_worst #all studies had <99.99% power to detect delta=0.5
n_worst_delta_0.8<-c(n_worst[1:5], rep(replicate_power$n_worst[5],2)) #11 largest studies had >99.99% power to detect delta=0.8


M <- c(1,2,3,4,5,6,7)
delta_0.2_large_n_worst<-power_sims(10000, M, 0.2, n_worst_delta_0.2, 7) 
delta_0.5_large_n_worst<-power_sims(10000, M, 0.5, n_worst_delta_0.5, 7) 
delta_0.8_large_n_worst<-power_sims(10000, M, 0.8, n_worst_delta_0.8, 7) 

rpe_power_results_worst <- rbind(delta_0.2_large_n_worst, delta_0.5_large_n_worst, delta_0.8_large_n_worst)
saveRDS(rpe_power_results_worst ,"rpe_power_results_worst.RDS")

rpe_power_results_worst$study <- "rpe"
rpp_power_results_worst$study <- "rpp"
RPP_RPE_worst <- rbind(rpe_power_results_worst, rpp_power_results_worst)
RPP_RPE_worst <- mutate(RPP_RPE_worst, prop = M/k)

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

 