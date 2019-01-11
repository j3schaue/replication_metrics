source("./src/metrics_funs.R")
library(tidyverse)
fulldat<-readRDS("C:/Users/Mena Whalen/Downloads/full_data_rep_fisher.RDS")
#data = paper from which results were pulled
#experiment = experiment name
#site = site name ("original" or "replicate")
#d = effect size estimate (Cohen's d)
#                         vd = variance of effect size estimate
#                        replicated = 0 if authors determined if replication failed, 1 if succeeded


ttestF<-function(theta){
  
  
}
theta<-0.2

fulldat %>% 
  mutate(theta = rep(theta, length(data))) %>%
  mutate(pval = pval_fun(d, vd) )%>%
  group_by(experiment, site) %>%
  spread(site, pval) %>%
  mutate(orig = sum(original, na.rm=T), rep=sum(replicate, na.rm=T)) %>%
  mutate(tstat = ifelse(orig <0.00001 & rep < 0.00001, NA, mean(orig - rep)/sd(orig - rep))) %>% 
  mutate(isSIG = abs(tstat) > 1.98)
  
  