subset <- full_data_rep_fisher[full_data_rep_fisher$data == "RPP" & full_data_rep_fisher$site == "replicate" & full_data_rep_fisher$replicated == 0,]
subset_RPP <- full_data_rep_fisher[full_data_rep_fisher$data == "RPP",]
subset_RPP_replicate <- full_data_rep_fisher[full_data_rep_fisher$data == "RPP" & full_data_rep_fisher$site == "replicate" ,]
subset$t <- subset$d / subset$vd
subset$significance <- ifelse(abs(subset$t)>1.96, 1, 0)
subset$n <- (8+subset$d^2)/(4*subset$vd)

rpp_data_cleaned <- read_csv("Fishers_method/rpp_data_cleaned.csv")
rpp$z.test <- rpp$d/sqrt(rpp$vd)
rpp_fishers <- subset(rpp_data_cleaned, rpp_data_cleaned$replicate == 1 & rpp_data_cleaned$pvalr > 0.05)

rpp_fishers$n_effective <- (8+(rpp_fishers$d)^2)/(4*rpp_fishers$vd)

#01-14-19
rpp_power_results_delta_0.2 <- readRDS("Fishers_method/rpp_power_results_delta_0.2.RDS")
rpp_power_results_delta_0.5 <- readRDS("Fishers_method/rpp_power_results_delta_0.5.RDS")
rpp_delta_0.5_fillin_power100 <- rpp_power_results_delta_0.2 %>% filter(M > max(rpp_power_results_delta_0.5$M)) %>% mutate(power = 1, delta = 0.5)
rpp_power_results_delta_0.8 <- readRDS("Fishers_method/rpp_power_results_delta_0.8.RDS")
rpp_delta_0.8_fillin_power100 <- rpp_power_results_delta_0.2 %>% filter(M > max(rpp_power_results_delta_0.8$M)) %>% mutate(power = 1, delta = 0.8)
rpp_power_results <- rbind(rpp_power_results_delta_0.2, rpp_power_results_delta_0.5, rpp_delta_0.5_fillin_power100, rpp_power_results_delta_0.8, rpp_delta_0.8_fillin_power100)

library(ggplot2)
n_labels <- c('25' = "n=25", '50' = "n=50", '75' = "n=75", '100'= "n=100")
k_labels <- c( '10' = "k=10", '50' = "k=50", '100' = "k=100")
rpp_power_results$prop <- rpp_power_results$M / rpp_power_results$k

library(wesanderson) # color palettes
zs1 = wes_palette("Zissou1")
dj1 = wes_palette("Darjeeling1")
dj2 = wes_palette("Darjeeling2")

# standard theme for plots
stdtheme = theme(panel.grid.minor = element_blank(),
                 axis.ticks = element_blank(),
                 axis.text = element_text(size = 16),
                 axis.title = element_text(size = 18),
                 legend.title = element_text(size = 16),
                 legend.text = element_text(size = 16),
                 legend.text.align = 0)

plot_rpp <- ggplot(rpp_power_results, aes(x = prop, y = power, color = factor(delta))) +
  stdtheme +
  scale_x_continuous(name = "Proportion of false negatives") +
  scale_y_continuous(name = "Power")+
  labs(title = "Power of Fisher's method: RPP", color = expression(delta))+
  geom_line()

rpe_power_results <- readRDS("Fishers_method/rpe_power_results.RDS")
rpe_delta_0.8_fillin_power100 <- rpe_power_results %>% filter(M > M[dim(rpe_power_results)[1]] & delta == 0.2) %>% mutate(power = 1, delta = 0.8)
rpe_power_results <- rbind(rpe_power_results, rpe_delta_0.8_fillin_power100)
rpe_power_results$prop <- rpe_power_results$M / rpe_power_results$k

rpe_power_results$study <- "RPE"
rpp_power_results$study <- "RPP"
all_power_results <- rbind(rpp_power_results, rpe_power_results)
saveRDS(all_power_results, "RPP_RPE_power_results.RDS")

study_labels <- c( 'RPE' = "RPE, k = 7", 'RPP' = "RPP, k = 64")

plot_all <- ggplot(all_power_results, aes(x = prop, y = power, color = factor(delta))) +
  facet_grid(rows = vars(study), labeller = labeller(study = study_labels)) +
  stdtheme +
  theme(strip.text = element_text(size = 16))+
  scale_x_continuous(name = "Proportion of false negatives") +
  scale_y_continuous(name = "Power")+
  labs(color = expression(delta))+
  geom_line()
