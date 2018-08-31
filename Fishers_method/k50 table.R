##get data in correct format
table_k50_n100 <- readRDS("~table_k50_n100.RDS")
table_k50_n25 <- readRDS("~table_k50_n25.RDS")
table_k50_n50 <- readRDS("~table_k50_n50.RDS")
table_k50_n75 <- readRDS("~table_k50_n75.RDS")

k50_n25_d.2 <- as.data.frame(table_k50_n25[,2])
colnames(k50_n25_d.2)[1] <- "power"
k50_n25_d.2$n <- rep(25,length(k50_n25_d.2))
k50_n25_d.2$k <- rep(50,length(k50_n25_d.2))
k50_n25_d.2$M <- c(1,2,3,4,5,10,25,50)
k50_n25_d.2$delta <- rep(0.2, dim(k50_n25_d.2)[1])

k50_n25_d.5 <- as.data.frame(table_k50_n25[,3])
colnames(k50_n25_d.5)[1] <- "power"
k50_n25_d.5$n <- rep(25,length(k50_n25_d.5))
k50_n25_d.5$k <- rep(50,length(k50_n25_d.5))
k50_n25_d.5$M <-c(1,2,3,4,5,10,25,50)
k50_n25_d.5$delta <- rep(0.5, dim(k50_n25_d.5)[1])

k50_n25_d.8 <- as.data.frame(table_k50_n25[,4])
colnames(k50_n25_d.8)[1] <- "power"
k50_n25_d.8$n <- rep(25,length(k50_n25_d.8))
k50_n25_d.8$k <- rep(50,length(k50_n25_d.8))
k50_n25_d.8$M <- c(1,2,3,4,5,10,25,50)
k50_n25_d.8$delta <- rep(0.8, dim(k50_n25_d.8)[1])


##########
k50_n50_d.2 <- as.data.frame(table_k50_n50[,2])
colnames(k50_n50_d.2)[1] <- "power"
k50_n50_d.2$n <- rep(50,length(k50_n50_d.2))
k50_n50_d.2$k <- rep(50,length(k50_n50_d.2))
k50_n50_d.2$M <- c(1,2,3,4,5,10,25,50)
k50_n50_d.2$delta <- rep(0.2, dim(k50_n50_d.2)[1])

k50_n50_d.5 <- as.data.frame(table_k50_n50[,3])
colnames(k50_n50_d.5)[1] <- "power"
k50_n50_d.5$n <- rep(50,length(k50_n50_d.5))
k50_n50_d.5$k <- rep(50,length(k50_n50_d.5))
k50_n50_d.5$M <- c(1,2,3,4,5,10,25,50)
k50_n50_d.5$delta <- rep(0.5, dim(k50_n50_d.5)[1])

k50_n50_d.8 <- as.data.frame(table_k50_n50[,4])
colnames(k50_n50_d.8)[1] <- "power"
k50_n50_d.8$n <- rep(50,length(k50_n50_d.8))
k50_n50_d.8$k <- rep(50,length(k50_n50_d.8))
k50_n50_d.8$M <- c(1,2,3,4,5,10,25,50)
k50_n50_d.8$delta <- rep(0.8, dim(k50_n50_d.8)[1])

########
k50_n75_d.2 <- as.data.frame(table_k50_n75[,2])
colnames(k50_n75_d.2)[1] <- "power"
k50_n75_d.2$n <- rep(75,length(k50_n75_d.2))
k50_n75_d.2$k <- rep(50,length(k50_n75_d.2))
k50_n75_d.2$M <- c(1,2,3,4,5,10,25,50)
k50_n75_d.2$delta <- rep(0.2, dim(k50_n75_d.2)[1])

k50_n75_d.5 <- as.data.frame(table_k50_n75[,3])
colnames(k50_n75_d.5)[1] <- "power"
k50_n75_d.5$n <- rep(75,length(k50_n75_d.5))
k50_n75_d.5$k <- rep(50,length(k50_n75_d.5))
k50_n75_d.5$M <- c(1,2,3,4,5,10,25,50)
k50_n75_d.5$delta <- rep(0.5, dim(k50_n75_d.5)[1])

k50_n75_d.8 <- as.data.frame(table_k50_n75[,4])
colnames(k50_n75_d.8)[1] <- "power"
k50_n75_d.8$n <- rep(75,length(k50_n75_d.8))
k50_n75_d.8$k <- rep(50,length(k50_n75_d.8))
k50_n75_d.8$M <- c(1,2,3,4,5,10,25,50)
k50_n75_d.8$delta <- rep(0.8, dim(k50_n75_d.8)[1])

########
k50_n100_d.2 <- as.data.frame(table_k50_n100[,2])
colnames(k50_n100_d.2)[1] <- "power"
k50_n100_d.2$n <- rep(100,length(k50_n100_d.2))
k50_n100_d.2$k <- rep(50,length(k50_n100_d.2))
k50_n100_d.2$M <- c(1,2,3,4,5,10,25,50)
k50_n100_d.2$delta <- rep(0.2, dim(k50_n100_d.2)[1])

k50_n100_d.5 <- as.data.frame(table_k50_n100[,3])
colnames(k50_n100_d.5)[1] <- "power"
k50_n100_d.5$n <- rep(100,length(k50_n100_d.5))
k50_n100_d.5$k <- rep(50,length(k50_n100_d.5))
k50_n100_d.5$M <- c(1,2,3,4,5,10,25,50)
k50_n100_d.5$delta <- rep(0.5, dim(k50_n100_d.5)[1])

k50_n100_d.8 <- as.data.frame(table_k50_n100[,4])
colnames(k50_n100_d.8)[1] <- "power"
k50_n100_d.8$n <- rep(100,length(k50_n100_d.8))
k50_n100_d.8$k <- rep(50,length(k50_n100_d.8))
k50_n100_d.8$M <- c(1,2,3,4,5,10,25,50)
k50_n100_d.8$delta <- rep(0.8, dim(k50_n100_d.8)[1])

k50 <- rbind(k50_n25_d.2,k50_n25_d.5,k50_n25_d.8,k50_n50_d.2,k50_n50_d.5,k50_n50_d.8,k50_n75_d.2, k50_n75_d.5, k50_n75_d.8,k50_n100_d.2,k50_n100_d.5,k50_n100_d.8)
saveRDS(k50,"k50_data.RDS")

library(ggplot2)
labels <- c('25' = "n=25", '50' = "n=50", '75' = "n=75", '100'= "n=100")
ggplot(k50, aes(x = M, y = power, color = factor(delta))) +
  theme(panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer(palette="Dark2")+
  scale_x_continuous(name = "# of false negatives", limits = c(1,50), breaks = seq(0,50,10)) +
  scale_y_continuous(name = "Power", limits = c(0,1), breaks = seq(0,1,0.1))+
  geom_point(size = 3) +
  facet_grid(.~n, labeller = labeller(n = labels)) +
  labs(title = "Power of Fisher's method for k=50 studies", color = expression(delta))+
  geom_smooth(method = "loess", se = FALSE, size = 0.5)
  #geom_line()


####NEW SIMS TO FILL IN GAPS###
k50_n25_d.2_extra <- power_sims(100000, c(15,20,30,35,40,45), 0.2, rep(25, 50), 50)
k50_n25_d.5_extra <- power_sims(100000, c(15,20,30,35,40,45), 0.5, rep(25, 50), 50)
k50_n25_d.8_extra <- power_sims(100000, c(15,20), 0.8, rep(25, 50), 50)

k50_n50_d.2_extra <- power_sims(100000, c(15,20,30,35,40,45), 0.2, rep(50, 50), 50)
k50_n50_d.5_extra <- power_sims(100000, c(15,20), 0.5, rep(50, 50), 50)
k50_n50_d.8_extra <- power_sims(100000, c(15,20), 0.8, rep(50, 50), 50) 

k50_n75_d.2_extra <- power_sims(100000, c(15,20,30,35,40,45), 0.2, rep(75, 50), 50)
k50_n75_d.5_extra <- power_sims(100000, c(15,20), 0.5, rep(75, 50), 50)
k50_n75_d.8_extra <- power_sims(100000, c(15,20), 0.8, rep(75, 50), 50) 

k50_n100_d.2_extra <- power_sims(100000, c(15,20,30,35,40,45), 0.2, rep(100, 50), 50)
k50_n100_d.5_extra <- power_sims(100000, c(15,20), 0.5, rep(100, 50), 50) 
k50_n100_d.8_extra <- power_sims(100000, c(15), 0.8, rep(100, 50), 50) 

k50 <- readRDS("./k50_data.RDS")
k50_list <- list(k50, k50_n25_d.2_extra,k50_n25_d.5_extra, k50_n25_d.8_extra, k50_n50_d.2_extra, k50_n50_d.5_extra, k50_n50_d.8_extra,k50_n75_d.2_extra,k50_n75_d.5_extra,k50_n75_d.8_extra, k50_n100_d.2_extra,k50_n100_d.5_extra,k50_n100_d.8_extra)
ChangeNames <- function(x) {
  names(x)[1] <- "power"
  return(x)
}
k50_list <- lapply(k50_list, ChangeNames)
k50_data <- ldply(k50_list, data.frame)
saveRDS(k50_data,"k50_data.RDS")
