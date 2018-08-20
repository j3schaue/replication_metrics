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
#n=100
n<-rep(100,10)
power_d.2_k10_n100 <-power_sims(100000,M,0.2,n,10)
power_d.5_k10_n100<-power_sims(100000,M,0.5,n,10)
power_d.8_k10_n100<-power_sims(100000,M,0.8,n,10)
table_k10_n100<-data.frame(M,power_d.2_k10_n100,power_d.5_k10_n100,power_d.8_k10_n100)
saveRDS(table_k10_n100,"table_k10_n100.RDS")

##get data in correct format
table_k10_n100 <- readRDS("./table_k10_n100.RDS")
table_k10_n25 <- readRDS("./table_k10_n25.RDS")
table_k10_n50 <- readRDS("./table_k10_n50.RDS")
table_k10_n75 <- readRDS("./table_k10_n75.RDS")

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
saveRDS(k10,"k10_data.RDS")
