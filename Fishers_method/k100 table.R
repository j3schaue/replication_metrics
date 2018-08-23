##k=100##
M<-c(1,2,3,4,5,10,50,100)

#n=25
n<-rep(25,100)
power_delta_0.2_k100_n25 <-power_sims(100000,M,0.2,n,100)
power_delta_0.5_k100_n25<-power_sims(100000,M,0.5,n,100)
power_delta_0.8_k100_n25<-power_sims(100000,M,0.8,n,100)
table_k100_n25<-data.frame(M,power_delta_0.2_k100_n25,power_delta_0.5_k100_n25,power_delta_0.8_k100_n25)
saveRDS(table_k100_n25,"table_k100_n25.RDS")

#n=50
n<-rep(50,100)
power_delta_0.2_k100_n50 <-power_sims(100000,M,0.2,n,100)
power_delta_0.5_k100_n50<-power_sims(100000,M,0.5,n,100)
power_delta_0.8_k100_n50<-power_sims(100000,M,0.8,n,100)
table_k100_n50<-data.frame(M,power_delta_0.2_k100_n50,power_delta_0.5_k100_n50,power_delta_0.8_k100_n50)
saveRDS(table_k100_n50,"table_k100_n50.RDS")

#n=75
n<-rep(75,100)
power_delta_0.2_k100_n75 <-power_sims(100000,M,0.2,n,100)
power_delta_0.5_k100_n75<-power_sims(100000,M,0.5,n,100)
power_delta_0.8_k100_n75<-power_sims(100000,M,0.8,n,100)
table_k100_n75<-data.frame(M,power_delta_0.2_k100_n75,power_delta_0.5_k100_n75,power_delta_0.8_k100_n75)
saveRDS(table_k100_n75,"table_k100_n75.RDS")

#n=100
n<-rep(100,100)
power_delta_0.2_k100_n100 <-power_sims(100000,M,0.2,n,100)
power_delta_0.5_k100_n100<-power_sims(100000,M,0.5,n,100)
power_delta_0.8_k100_n100<-power_sims(100000,M,0.8,n,100)
table_k100_n100<-data.frame(M,power_delta_0.2_k100_n100,power_delta_0.5_k100_n100,power_delta_0.8_k100_n100)
saveRDS(table_k100_n100,"table_k100_n100.RDS")





##get data in correct format
table_k100_n100 <- readRDS("~table_k100_n100.RDS")
table_k100_n25 <- readRDS("~table_k100_n25.RDS")
table_k100_n50 <- readRDS("~table_k100_n50.RDS")
table_k100_n75 <- readRDS("~table_k100_n75.RDS")

k100_n25_d.2 <- as.data.frame(table_k100_n25[,2])
colnames(k100_n25_d.2)[1] <- "power"
k100_n25_d.2$n <- rep(25,length(k100_n25_d.2))
k100_n25_d.2$k <- rep(100,length(k100_n25_d.2))
k100_n25_d.2$M <- c(1,2,3,4,5,10,50,100)
k100_n25_d.2$delta <- rep(0.2, dim(k100_n25_d.2)[1])

k100_n25_d.5 <- as.data.frame(table_k100_n25[,3])
colnames(k100_n25_d.5)[1] <- "power"
k100_n25_d.5$n <- rep(25,length(k100_n25_d.5))
k100_n25_d.5$k <- rep(100,length(k100_n25_d.5))
k100_n25_d.5$M <-c(1,2,3,4,5,10,50,100)
k100_n25_d.5$delta <- rep(0.5, dim(k100_n25_d.5)[1])

k100_n25_d.8 <- as.data.frame(table_k100_n25[,4])
colnames(k100_n25_d.8)[1] <- "power"
k100_n25_d.8$n <- rep(25,length(k100_n25_d.8))
k100_n25_d.8$k <- rep(100,length(k100_n25_d.8))
k100_n25_d.8$M <- c(1,2,3,4,5,10,50,100)
k100_n25_d.8$delta <- rep(0.8, dim(k100_n25_d.8)[1])


##########
k100_n50_d.2 <- as.data.frame(table_k100_n50[,2])
colnames(k100_n50_d.2)[1] <- "power"
k100_n50_d.2$n <- rep(50,length(k100_n50_d.2))
k100_n50_d.2$k <- rep(100,length(k100_n50_d.2))
k100_n50_d.2$M <- c(1,2,3,4,5,10,50,100)
k100_n50_d.2$delta <- rep(0.2, dim(k100_n50_d.2)[1])

k100_n50_d.5 <- as.data.frame(table_k100_n50[,3])
colnames(k100_n50_d.5)[1] <- "power"
k100_n50_d.5$n <- rep(50,length(k100_n50_d.5))
k100_n50_d.5$k <- rep(100,length(k100_n50_d.5))
k100_n50_d.5$M <- c(1,2,3,4,5,10,50,100)
k100_n50_d.5$delta <- rep(0.5, dim(k100_n50_d.5)[1])

k100_n50_d.8 <- as.data.frame(table_k100_n50[,4])
colnames(k100_n50_d.8)[1] <- "power"
k100_n50_d.8$n <- rep(50,length(k100_n50_d.8))
k100_n50_d.8$k <- rep(100,length(k100_n50_d.8))
k100_n50_d.8$M <- c(1,2,3,4,5,10,50,100)
k100_n50_d.8$delta <- rep(0.8, dim(k100_n50_d.8)[1])

########
k100_n75_d.2 <- as.data.frame(table_k100_n75[,2])
colnames(k100_n75_d.2)[1] <- "power"
k100_n75_d.2$n <- rep(75,length(k100_n75_d.2))
k100_n75_d.2$k <- rep(100,length(k100_n75_d.2))
k100_n75_d.2$M <- c(1,2,3,4,5,10,50,100)
k100_n75_d.2$delta <- rep(0.2, dim(k100_n75_d.2)[1])

k100_n75_d.5 <- as.data.frame(table_k100_n75[,3])
colnames(k100_n75_d.5)[1] <- "power"
k100_n75_d.5$n <- rep(75,length(k100_n75_d.5))
k100_n75_d.5$k <- rep(100,length(k100_n75_d.5))
k100_n75_d.5$M <- c(1,2,3,4,5,10,50,100)
k100_n75_d.5$delta <- rep(0.5, dim(k100_n75_d.5)[1])

k100_n75_d.8 <- as.data.frame(table_k100_n75[,4])
colnames(k100_n75_d.8)[1] <- "power"
k100_n75_d.8$n <- rep(75,length(k100_n75_d.8))
k100_n75_d.8$k <- rep(100,length(k100_n75_d.8))
k100_n75_d.8$M <- c(1,2,3,4,5,10,50,100)
k100_n75_d.8$delta <- rep(0.8, dim(k100_n75_d.8)[1])

########
k100_n100_d.2 <- as.data.frame(table_k100_n100[,2])
colnames(k100_n100_d.2)[1] <- "power"
k100_n100_d.2$n <- rep(100,length(k100_n100_d.2))
k100_n100_d.2$k <- rep(100,length(k100_n100_d.2))
k100_n100_d.2$M <- c(1,2,3,4,5,10,50,100)
k100_n100_d.2$delta <- rep(0.2, dim(k100_n100_d.2)[1])

k100_n100_d.5 <- as.data.frame(table_k100_n100[,3])
colnames(k100_n100_d.5)[1] <- "power"
k100_n100_d.5$n <- rep(100,length(k100_n100_d.5))
k100_n100_d.5$k <- rep(100,length(k100_n100_d.5))
k100_n100_d.5$M <- c(1,2,3,4,5,10,50,100)
k100_n100_d.5$delta <- rep(0.5, dim(k100_n100_d.5)[1])

k100_n100_d.8 <- as.data.frame(table_k100_n100[,4])
colnames(k100_n100_d.8)[1] <- "power"
k100_n100_d.8$n <- rep(100,length(k100_n100_d.8))
k100_n100_d.8$k <- rep(100,length(k100_n100_d.8))
k100_n100_d.8$M <- c(1,2,3,4,5,10,50,100)
k100_n100_d.8$delta <- rep(0.8, dim(k100_n100_d.8)[1])

k100 <- rbind(k100_n25_d.2,k100_n25_d.5,k100_n25_d.8,k100_n50_d.2,k100_n50_d.5,k100_n50_d.8,k100_n75_d.2, k100_n75_d.5, k100_n75_d.8,k100_n100_d.2,k100_n100_d.5,k100_n100_d.8)
saveRDS(k100,"k100_data.RDS")

library(ggplot2)
labels <- c('25' = "n=25", '50' = "n=50", '75' = "n=75", '100'= "n=100")
ggplot(k100, aes(x = M, y = power, color = factor(delta))) +
  theme(panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer(palette="Dark2")+
  scale_x_continuous(name = "# of false negatives", limits = c(1,100), breaks = seq(0,100,10)) +
  scale_y_continuous(name = "Power", limits = c(0,1), breaks = seq(0,1,0.1))+
  #geom_point(size = 3) +
  facet_grid(.~n, labeller = labeller(n = labels)) +
  labs(title = "Power of Fisher's method for k=100 studies", color = expression(delta))+
  #geom_smooth(method = "loess", se = FALSE, size = 0.5)
  geom_line()


################ EXTRA CODE
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

##ORIGINAL SEPARTE PLOT CODE

plot_k10 <- ggplot(k10, aes(x = prop, y = power, color = factor(delta))) +
  theme(panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer(palette="Dark2")+
  scale_x_continuous(name = "# of false negatives", limits = c(0,1), breaks = seq(0,1,.1)) +
  scale_y_continuous(name = "Power", limits = c(0,1), breaks = seq(0,1,0.1))+
  #geom_point(size = 3) +
  facet_grid(.~n, labeller = labeller(n = labels)) +
  labs(title = "Power of Fisher's method for k=10 studies", color = expression(delta))+
  geom_smooth(method = "loess", se = FALSE, size = 0.5)

plot_k50 <- ggplot(k50, aes(x = prop, y = power, color = factor(delta))) +
  theme(panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer(palette="Dark2")+
  scale_x_continuous(name = "# of false negatives", limits = c(0,1), breaks = seq(0,1,.10)) +
  scale_y_continuous(name = "Power", limits = c(0,1), breaks = seq(0,1,0.1))+
  #geom_point(size = 3) +
  facet_grid(.~n, labeller = labeller(n = labels)) +
  labs(title = "Power of Fisher's method for k=50 studies", color = expression(delta))+
  #geom_smooth(method = "loess", se = FALSE, size = 0.5)
  geom_line()

plot_k100 <- ggplot(k100, aes(x = prop, y = power, color = factor(delta))) +
  theme(panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer(palette="Dark2")+
  scale_x_continuous(name = "# of false negatives", limits = c(0,1), breaks = seq(0,1,.10)) +
  scale_y_continuous(name = "Power", limits = c(0,1), breaks = seq(0,1,0.1))+
  #geom_point(size = 3) +
  facet_grid(.~n, labeller = labeller(n = labels)) +
  labs(title = "Power of Fisher's method for k=100 studies", color = expression(delta))+
  #geom_smooth(method = "loess", se = FALSE, size = 0.5)
  geom_line()

library(gridExtra)
grid.arrange(plot_k10, plot_k50, plot_k100, nrow = 3)

