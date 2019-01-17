source("./src/metrics_funs.R")
library(tidyverse)
library(readr)
rpp<- read_csv("Ttest-pvals/rpp_data_cleaned.csv")
fulldat<-readRDS("C:/Users/Mena Whalen/Downloads/full_data_rep_fisher.RDS")
fulldat<- readRDS("C:/Users/Mena/Documents/Replication/replication_metrics/Ttest-pvals/full_data_rep_fisher.RDS")
#data = paper from which results were pulled
#experiment = experiment name
#site = site name ("original" or "replicate")
#d = effect size estimate (Cohen's d)
#                         vd = variance of effect size estimate
#                        replicated = 0 if authors determined if replication failed, 1 if succeeded

# t test function for the same theta
rpp$site<-ifelse(rpp$replicate == 0, "original", "replicate")
newrpp<-data.frame(cbind(data = rep("RPP",length(rpp$site)),experiment =rpp$experiment, site= rpp$site, d=rpp$d, 
                         vd=rpp$vd, replicated=rpp$replicated))

fulldat<-fulldat[!(fulldat$data == "RPP"),]
fulldat<-rbind(fulldat, newrpp)
fulldat$d<-as.numeric(fulldat$d)
fulldat$vd<-as.numeric(fulldat$vd)
fulldat$replicated<-as.integer(fulldat$replicated)


ttestsameFunc<-function(theta){
  
fulldat %>% 
  mutate(theta = rep(theta, length(data))) %>%
  mutate(pval = pval_fun(theta, vd) )%>%
  group_by(data,experiment) %>%
  spread(site, pval) %>%
  mutate(orig = sum(original, na.rm=T), rep=sum(replicate, na.rm=T)) %>%
  mutate(diff = orig - rep) %>%
  distinct(data, experiment, orig, rep, diff) %>%
  ungroup() %>%
  group_by(data) %>%
  summarise( n = length(data),
             mean = mean(diff),
             sd = sd(diff),
             power = power.t.test(n=n, delta = mean(diff), sd = sd(diff), sig.level = 0.05, type = "paired", alternative = "two.sided")$power
             )
}

ttestsameFunc(0.2)
ttestsameFunc(0.5)
ttestsameFunc(0.8)



####################################################################################
# t test function for different thetas
##bigger effects go into theta1


ttestdiffFunc<-function(theta1, theta2){
  fulldat %>% 
    mutate(theta = ifelse(site == "original", theta1, theta2)) %>%
    mutate(pval = pval_fun(theta, vd)) %>%
    group_by(data,experiment) %>%
    spread(site, pval) %>%
    mutate(orig = sum(original, na.rm=T), rep=sum(replicate, na.rm=T)) %>%
    mutate(diff = orig - rep) %>%
    distinct(data, experiment, orig, rep, diff) %>%
    ungroup() %>%
    group_by(data) %>%
    summarise( n = length(data),
               mean = mean(diff),
               sd = sd(diff),
               power = 1-power.t.test(n=n, delta = mean(diff), sd = sd(diff), sig.level = 0.05, type = "paired", alternative = "two.sided")$power
    )
  
  
}

ttestdiffFunc(0.8,0)
ttestdiffFunc(0.8,0.2)
ttestdiffFunc(0.8,0.5)
ttestdiffFunc(0.5,0)
ttestdiffFunc(0.5,0.2)
ttestdiffFunc(0.2,0)
