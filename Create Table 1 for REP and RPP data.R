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
