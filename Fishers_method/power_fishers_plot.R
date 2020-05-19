##find sample sizes needed to get 40, 60, 80% power
calculate_v <- function(n, delta){
  v <- 2/n + delta^2/(4*n)
  return(v)
}
power_fun(.8, calculate_v(27,.8)) #.81 power
power_fun(.5, calculate_v(65,.5)) #.8 power
power_fun(.2, calculate_v(400,.2))#.8 power
power_fun(.8, calculate_v(17,.8))#.61
power_fun(.5, calculate_v(40,.5))#.6
power_fun(.2, calculate_v(250,.2))#.6
power_fun(.8, calculate_v(10,.8))#.4
power_fun(.5, calculate_v(24,.5))#.4
power_fun(.2, calculate_v(150,.2))#.4

##k=10##
M<-c(1,2,3,4,5,10)
power_by_k <- function(k, M){
  #M <- c(1, seq(.1*k,k,.1*k))  
  
  #40% power
  n_0.5 <- rep(24, k)
  power_40_delta_0.5 <- power_sims(100000,M,0.5,n_0.5,k)
  library(dplyr)
  power_40 <- mutate(power_40_delta_0.5, prop = M/k)
  power_40 <- mutate(power_40, power_original = .4)
  
  #60% power
  n_0.5 <- rep(40, k)
  power_60_delta_0.5 <- power_sims(100000,M,0.5,n_0.5,k)
  power_60 <- mutate(power_60_delta_0.5, prop = M/k)
  power_60 <- mutate(power_60, power_original = 0.6)
  
  
  #80% power
  n_0.5 <- rep(65, k)
  power_80_delta_0.5 <- power_sims(100000,M,0.5,n_0.5,k)
  power_80 <- mutate(power_80_delta_0.5, prop = M/k)
  power_80 <- mutate(power_80, power_original = 0.8)
  
  power_fishers <- rbind(power_40, power_60, power_80)
  return(power_fishers)
}
k_10 <- power_by_k(10, seq(1,10,1))
k_50 <- power_by_k(50, seq(1,50,1))
k_100 <- power_by_k(100, seq(2,100,2))

power_fishers <- rbind(k_10, k_50, k_100)
extra <- data.frame(power = rep(1,31), 
                              k = c(rep(50,13), rep(100, 18)),
                              M = c(45, 50, 
                                    30, 35, 40, 45, 50,
                                    25, 30, 35, 40, 45, 50,
                                    60, 70, 80, 90, 100,
                                    50, 60, 70, 80, 90, 100,
                                    40, 50, 60, 70, 80, 90, 100),
                              delta = rep(0.5,31), 
                              prop = c(.9, 1.0, 
                                       .6, .7, .8, .9, 1.0,
                                       .5,.6, .7, .8, .9, 1.0,
                                       .6, .7, .8, .9, 1.0,
                                       .5,.6, .7, .8, .9, 1.0,
                                       .4, .5,.6, .7, .8, .9, 1.0),
                              power_original = c(0.4, 0.4, 
                                                 0.6, 0.6, 0.6, 0.6, 0.6,
                                                 0.8, 0.8, 0.8, 0.8, 0.8, 0.8,
                                                 0.4, 0.4, 0.4, 0.4, 0.4,
                                                 0.6, 0.6, 0.6, 0.6, 0.6, 0.6,
                                                 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8))
power_fishers <- rbind(power_fishers, extra)
r_labels <- c( '10' = "r = 10", '50' = "r = 50", '100' = "r = 100")
saveRDS(power_fishers, "power_fishers.RDS")
test <- readRDS("power_fishers.RDS")

ggplot(power_fishers, aes(x = prop, y = power, linetype = factor(power_original))) +
  facet_wrap(~k, labeller = labeller(k = r_labels), ncol = 1) +
  stdtheme + 
  theme(axis.text = element_text(size = 16), strip.text = element_text(size = 16)) +
  scale_x_continuous(name = "Proportion of false negatives" ) +
  scale_y_continuous(name = "Power of Fisher's method", breaks = seq(0,1,.25)) +
  labs(linetype = "Power of \nexperiments")+
  geom_line()
  #geom_smooth(se = F, color = "black")





