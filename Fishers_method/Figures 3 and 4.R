library(ggplot2)
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

#Figure 3
power_fishers <- readRDS("power_fishers.RDS")

ggplot(power_fishers, aes(x = prop, y = power, linetype = factor(power_original))) +
  facet_wrap(~k, labeller = labeller(k = r_labels), ncol = 1) +
  stdtheme + 
  theme(axis.text = element_text(size = 16), strip.text = element_text(size = 16)) +
  scale_x_continuous(name = "Proportion of false negatives" ) +
  scale_y_continuous(name = "Power of Fisher's method", breaks = seq(0, 1, .25)) +
  labs(linetype = "Power of \nexperiments")+
  geom_line()
  #geom_smooth(se = F, color = "black")

#Figure 4
RPP_RPE_power_results <- readRDS("RPP_RPE_power_results.RDS")
study_labels <- c( 'RPE' = "RPE, r = 7", 'RPP' = "RPP, r = 64")

ggplot(RPP_RPE_power_results, aes(x = prop, y = power, linetype = factor(delta)))+
  facet_grid(rows = vars(study), labeller = labeller(study = study_labels)) +
  stdtheme +
  theme(strip.text = element_text(size = 16))+
  scale_x_continuous(name = "Proportion of false negatives" ) +
  scale_y_continuous(name = "Power of Fisher's method")+
  labs(linetype = expression(delta))+
  geom_line()
  #geom_smooth(se = FALSE, color = "black")
