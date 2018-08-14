power_sims<-function(N,M,delta,n,K){
  #########################################################################################
  # TAKES: N; number of simulations 
  #        M; vector of number of non-null effects
  #        delta; effect size under alternative hypothesis, on scale of cohen's d
  #        n; vector of treatment/control sample size across studies (total sample size/2)
  #        K; number of studies
  # RETURNS: power of Fisher's test to reject
  # Assumes 2-sided p-values, throws away p-values<=0.05 to match OSC methods
  #########################################################################################
  
  T<-c() #empty list to store Fisher's test statistic 
  Power<-matrix() #empty matrix to store results
  
  for(k in 1:length(M)){
    
    for (i in 1:N){
      
      print(i) # can uncomment to show progress for lengthy simulations
    
      p0<-runif(K - M[k], 0.05, 1) #draws p-values for the true null effects
      
      p1<-c() #create list to store p-values drawn for non-null effects
      
      for (j in 1:M[k]){ 
        
        p1[j]<-0
        
        while (p1[j] <= 0.05) { #throw away p-values<=0.05
          p1[j]<-2*(1 - pnorm(abs(rnorm(1, delta, sqrt(2/n[j] + delta^2/(4*n[j]))))/sqrt(2/n[j] + delta^2/(4*n[j]))))
        }
        
        print(n[j]) # can uncomment to show progress for lengthy simulations 
        }
      
      #test statistic for Fisher's method, with transformation for truncating p-values
      T[i]<--2*sum(log((p0-0.05)/0.95))-2*sum(log((p1-0.05)/0.95)) 
    
      }
    
    Power[k]<-sum(T > qchisq(0.95, 2*K) )/N
  
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

###NEW TABLES###

##k=10##
M<-c(1,2,3,4,5,10)

#n=25
n<-rep(25,10)
power_delta_0.2_k10_n25 <-power_sims(100000,M,0.2,n,10)
power_delta_0.5_k10_n25<-power_sims(100000,M,0.5,n,10)
power_delta_0.8_k10_n25<-power_sims(100000,M,0.8,n,10)
table_k10_n25<-data.frame(M,power_delta_0.2_k10_n25,power_delta_0.5_k10_n25,power_delta_0.8_k10_n25)
saveRDS(table_k10_n25,"table_k10_n25.RDS")
#n=50
n<-rep(50,10)
power_delta_0.2_k10 <-power_sims(100000,M,0.2,n,10)
power_delta_0.5_k10<-power_sims(100000,M,0.5,n,10)
power_delta_0.8_k10<-power_sims(100000,M,0.8,n,10)
table_k10_n50<-data.frame(M,power_delta_0.2_k10,power_delta_0.5_k10,power_delta_0.8_k10)
saveRDS(table_k10_n50,"table_k10_n50.RDS")
#n=75
n<-rep(75,10)
power_delta_0.2_k10_n75 <-power_sims(100000,M,0.2,n,10)
power_delta_0.5_k10_n75<-power_sims(100000,M,0.5,n,10)
power_delta_0.8_k10_n75<-power_sims(100000,M,0.8,n,10)
table_k10_n75<-data.frame(M,power_delta_0.2_k10_n75,power_delta_0.5_k10_n75,power_delta_0.8_k10_n75)
saveRDS(table_k10_n75,"table_k10_n75.RDS")
saveRDS(power_delta_0.2_k10_n75,"power_delta_0.2_k10_n75.RDS")
saveRDS(power_delta_0.5_k10_n75,"power_delta_0.5_k10_n75.RDS")
#n=100
n<-rep(100,10)
power_d.2_k10_n100 <-power_sims(100000,M,0.2,n,10)
power_d.5_k10_n100<-power_sims(100000,M,0.5,n,10)
power_d.8_k10_n100<-power_sims(100000,M,0.8,n,10)
table_k10_n100<-data.frame(M,power_d.2_k10_n100,power_d.5_k10_n100,power_d.8_k10_n100)
saveRDS(table_k10_n100,"table_k10_n100.RDS")

##k=100##
M<-c(1,2,3,4,5,10,50,100)

#n=25
n<-rep(25,100)
power_delta_0.2_k100_n25 <-power_sims(100000,M,0.2,n,100)
power_delta_0.5_k100_n25<-power_sims(100000,M,0.5,n,100)
power_delta_0.8_k100_n25<-power_sims(100000,M,0.8,n,100)
table_k100_n25<-data.frame(M,power_delta_0.2_k100_n25,power_delta_0.5_k100_n25,power_delta_0.8_k100_n25)
saveRDS(power_delta_0.2_k100_n25,"power_delta_0.2_k100_n25.RDS")
saveRDS(power_delta_0.5_k100_n25,"power_delta_0.5_k100_n25.RDS")
saveRDS(power_delta_0.8_k100_n25,"power_delta_0.8_k100_n25.RDS")
saveRDS(table_k100_n25,"table_k100_n25.RDS")

#n=50
n<-rep(50,100)
power_delta_0.2_k100_n50 <-power_sims(100000,M,0.2,n,100)
saveRDS(power_delta_0.2_k100_n50,"power_delta_0.2_k100_n50.RDS")
power_delta_0.5_k100_n50<-power_sims(100000,M,0.5,n,100)
saveRDS(power_delta_0.5_k100_n50,"power_delta_0.5_k100_n50.RDS")
power_delta_0.8_k100_n50<-power_sims(100000,M,0.8,n,100)
saveRDS(power_delta_0.8_k100_n50,"power_delta_0.8_k100_n50.RDS")
table_k100_n50<-data.frame(M,power_delta_0.2_k100_n50,power_delta_0.5_k100_n50,power_delta_0.8_k100_n50)
saveRDS(table_k100_n50,"table_k100_n50.RDS")

#n=75
n<-rep(75,100)
power_delta_0.2_k100_n75 <-power_sims(100000,M,0.2,n,100)
saveRDS(power_delta_0.2_k100_n75,"power_delta_0.2_k100_n75.RDS")
power_delta_0.5_k100_n75<-power_sims(100000,M,0.5,n,100)
saveRDS(power_delta_0.5_k100_n75,"power_delta_0.5_k100_n75.RDS")
power_delta_0.8_k100_n75<-power_sims(100000,M,0.8,n,100)
saveRDS(power_delta_0.8_k100_n75,"power_delta_0.8_k100_n75.RDS")
table_k100_n75<-data.frame(M,power_delta_0.2_k100_n75,power_delta_0.5_k100_n75,power_delta_0.8_k100_n75)
saveRDS(table_k100_n75,"table_k100_n75.RDS")

#n=100
n<-rep(100,100)
power_delta_0.8_k100_n100_M1 <-power_sims(100000,1,0.8,n,100)
saveRDS(power_delta_0.8_k100_n100_M1,"power_delta_0.8_k100_n100_M1.RDS")
power_delta_0.8_k100_n100_M2 <-power_sims(100000,2,0.8,n,100)
saveRDS(power_delta_0.8_k100_n100_M2,"power_delta_0.8_k100_n100_M2.RDS")
power_delta_0.8_k100_n100_M3 <-power_sims(100000,3,0.8,n,100)
saveRDS(power_delta_0.8_k100_n100_M3,"power_delta_0.8_k100_n100_M3.RDS")
power_delta_0.8_k100_n100_M4 <-power_sims(100000,4,0.8,n,100)
saveRDS(power_delta_0.8_k100_n100_M4,"power_delta_0.8_k100_n100_M4.RDS")
power_delta_0.8_k100_n100_M5 <-power_sims(100000,5,0.8,n,100)
saveRDS(power_delta_0.8_k100_n100_M5,"power_delta_0.8_k100_n100_M5.RDS")
power_delta_0.8_k100_n100_M10 <-power_sims(100000,10,0.8,n,100)
saveRDS(power_delta_0.8_k100_n100_M10,"power_delta_0.8_k100_n100_M10.RDS")
power_delta_0.8_k100_n100_M50 <-power_sims(100000,50,0.8,n,100)
saveRDS(power_delta_0.8_k100_n100_M50,"power_delta_0.8_k100_n100_M50.RDS")
power_delta_0.8_k100_n100_M100 <-power_sims(1000,100,0.8,n,100)
saveRDS(power_delta_0.8_k100_n100_M100,"power_delta_0.8_k100_n100_M100.RDS")

power_delta_0.8_k100_n100 <- c(power_delta_0.8_k100_n100_M1, power_delta_0.8_k100_n100_M1, power_delta_0.8_k100_n100_M3, power_delta_0.8_k100_n100_M4, power_delta_0.8_k100_n100_M5, power_delta_0.8_k100_n100_M10, power_delta_0.8_k100_n100_M50, power_delta_0.8_k100_n100_M100)


power_delta_0.5_k100_n100<-power_sims(100000,M,0.5,n,100)
saveRDS(power_delta_0.5_k100_n100,"power_delta_0.5_k100_n100.RDS")
power_delta_0.8_k100_n100<-power_sims(100000,M,0.8,n,100)
saveRDS(power_delta_0.8_k100_n100,"power_delta_0.8_k100_n100.RDS")
table_k100_n100<-data.frame(M,power_delta_0.2_k100_n100,power_delta_0.5_k100_n100,power_delta_0.8_k100_n100)
saveRDS(table_k100_n100,"table_k100_n100.RDS")

##get data in correct format
table_k10_n100 <- readRDS("~table_k10_n100.RDS")
table_k10_n25 <- readRDS("~table_k10_n25.RDS")
table_k10_n50 <- readRDS("~table_k10_n50.RDS")
table_k10_n75 <- readRDS("~table_k10_n75.RDS")

k10_n25_d.2 <- as.data.frame(table_k10_n25[,2])
colnames(k10_n25_d.2)[1] <- "power"
k10_n25_d.2$n <- rep(25,length(k10_n25_d.2))
k10_n25_d.2$k <- rep(10,length(k10_n25_d.2))
k10_n25_d.2$M <- c(1,2,3,4,5,10)
k10_n25_d.2$delta <- rep(0.2, dim(k10_n25_d.2)[1])

k10_n25_d.5 <- as.data.frame(table_k10_n25[,3])
colnames(k10_n25_d.5)[1] <- "power"
k10_n25_d.5$n <- rep(25,length(k10_n25_d.5))
k10_n25_d.5$k <- rep(10,length(k10_n25_d.5))
k10_n25_d.5$M <- c(1,2,3,4,5,10)
k10_n25_d.5$delta <- rep(0.5, dim(k10_n25_d.5)[1])

k10_n25_d.8 <- as.data.frame(table_k10_n25[,4])
colnames(k10_n25_d.8)[1] <- "power"
k10_n25_d.8$n <- rep(25,length(k10_n25_d.8))
k10_n25_d.8$k <- rep(10,length(k10_n25_d.8))
k10_n25_d.8$M <- c(1,2,3,4,5,10)
k10_n25_d.8$delta <- rep(0.8, dim(k10_n25_d.8)[1])


##########
k10_n50_d.2 <- as.data.frame(table_k10_n50[,2])
colnames(k10_n50_d.2)[1] <- "power"
k10_n50_d.2$n <- rep(50,length(k10_n50_d.2))
k10_n50_d.2$k <- rep(10,length(k10_n50_d.2))
k10_n50_d.2$M <- c(1,2,3,4,5,10)
k10_n50_d.2$delta <- rep(0.2, dim(k10_n50_d.2)[1])

k10_n50_d.5 <- as.data.frame(table_k10_n50[,3])
colnames(k10_n50_d.5)[1] <- "power"
k10_n50_d.5$n <- rep(50,length(k10_n50_d.5))
k10_n50_d.5$k <- rep(10,length(k10_n50_d.5))
k10_n50_d.5$M <- c(1,2,3,4,5,10)
k10_n50_d.5$delta <- rep(0.5, dim(k10_n50_d.5)[1])

k10_n50_d.8 <- as.data.frame(table_k10_n50[,4])
colnames(k10_n50_d.8)[1] <- "power"
k10_n50_d.8$n <- rep(50,length(k10_n50_d.8))
k10_n50_d.8$k <- rep(10,length(k10_n50_d.8))
k10_n50_d.8$M <- c(1,2,3,4,5,10)
k10_n50_d.8$delta <- rep(0.8, dim(k10_n50_d.8)[1])

########
k10_n75_d.2 <- as.data.frame(table_k10_n75[,2])
colnames(k10_n75_d.2)[1] <- "power"
k10_n75_d.2$n <- rep(75,length(k10_n75_d.2))
k10_n75_d.2$k <- rep(10,length(k10_n75_d.2))
k10_n75_d.2$M <- c(1,2,3,4,5,10)
k10_n75_d.2$delta <- rep(0.2, dim(k10_n75_d.2)[1])

k10_n75_d.5 <- as.data.frame(table_k10_n75[,3])
colnames(k10_n75_d.5)[1] <- "power"
k10_n75_d.5$n <- rep(75,length(k10_n75_d.5))
k10_n75_d.5$k <- rep(10,length(k10_n75_d.5))
k10_n75_d.5$M <- c(1,2,3,4,5,10)
k10_n75_d.5$delta <- rep(0.5, dim(k10_n75_d.5)[1])

k10_n75_d.8 <- as.data.frame(table_k10_n75[,4])
colnames(k10_n75_d.8)[1] <- "power"
k10_n75_d.8$n <- rep(75,length(k10_n75_d.8))
k10_n75_d.8$k <- rep(10,length(k10_n75_d.8))
k10_n75_d.8$M <- c(1,2,3,4,5,10)
k10_n75_d.8$delta <- rep(0.8, dim(k10_n75_d.8)[1])

########
k10_n100_d.2 <- as.data.frame(table_k10_n100[,2])
colnames(k10_n100_d.2)[1] <- "power"
k10_n100_d.2$n <- rep(100,length(k10_n100_d.2))
k10_n100_d.2$k <- rep(10,length(k10_n100_d.2))
k10_n100_d.2$M <- c(1,2,3,4,5,10)
k10_n100_d.2$delta <- rep(0.2, dim(k10_n100_d.2)[1])

k10_n100_d.5 <- as.data.frame(table_k10_n100[,3])
colnames(k10_n100_d.5)[1] <- "power"
k10_n100_d.5$n <- rep(100,length(k10_n100_d.5))
k10_n100_d.5$k <- rep(10,length(k10_n100_d.5))
k10_n100_d.5$M <- c(1,2,3,4,5,10)
k10_n100_d.5$delta <- rep(0.5, dim(k10_n100_d.5)[1])

k10_n100_d.8 <- as.data.frame(table_k10_n100[,4])
colnames(k10_n100_d.8)[1] <- "power"
k10_n100_d.8$n <- rep(100,length(k10_n100_d.8))
k10_n100_d.8$k <- rep(10,length(k10_n100_d.8))
k10_n100_d.8$M <- c(1,2,3,4,5,10)
k10_n100_d.8$delta <- rep(0.8, dim(k10_n100_d.8)[1])

k10 <- rbind(k10_n25_d.2,k10_n25_d.5,k10_n25_d.8,k10_n50_d.2,k10_n50_d.5,k10_n50_d.8,k10_n75_d.2, k10_n75_d.5, k10_n75_d.8,k10_n100_d.2,k10_n100_d.5,k10_n100_d.8)

library(ggplot2)
labels <- c('25' = "n=25", '50' = "n=50", '75' = "n=75", '100'= "n=100")
ggplot(k10, aes(x = M, y = power, color = factor(delta))) +
  theme(panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer(palette="Dark2")+
  scale_x_continuous(name = "# of false negatives", limits = c(1,10), breaks = seq(1,10,1)) +
  scale_y_continuous(name = "Power", limits = c(0,1), breaks = seq(0,1,0.1))+
  #geom_point(size = 3) +
  facet_grid(.~n, labeller = labeller(n = labels)) +
  labs(title = "Power of Fisher's method for k=10 studies", color = expression(delta))+
  geom_smooth(method = "loess", se = FALSE, size = 0.5)

