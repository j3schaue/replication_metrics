
setwd(choose.dir())

# Load the RPP data.

rpp_dat <- read.csv("rpp_data.csv")

# Take the meta-analytic subset of the data.

meta_sub <- rpp_dat[is.na(rpp_dat$Meta.analytic.estimate..Fz.) == F,]

# Convert the correlation coefficients into Cohen's d. 

d.o <- (2*meta_sub$T_r..O.)/sqrt(1 - meta_sub$T_r..O.^2)
d.r <- (2*meta_sub$T_r..R.)/sqrt(1 - meta_sub$T_r..R.^2)

# Calculate the minimum detectable effect size given a certain power and the 
# sample size. (Do we use the observed sample size? Or the sample size needed for
# x% power?)

mdes <- function(power, n){
  (1.96 - qnorm(1-power))/sqrt(n)
}

# What is the MDES for replication studies @ 80% power?

mdes(0.8, meta_sub$T_N..R.) # observed sample size
mdes(0.8, as.numeric(meta_sub$X80..power)) # sample needed for 80% power

# What is the MDES for original studies @ 60% power? @ 40% power?

mdes(0.8, meta_sub$T_N..O.) # using the observed sample size
mdes(0.6, meta_sub$T_N..O.) # using the observed sample size
mdes(0.4, meta_sub$T_N..O.) # using the observed sample size

# So, I suppose we want to know the difference between mdes for original and
# replication studies. 

mdes(0.8, meta_sub$T_N..O.) - mdes(0.8, meta_sub$T_N..R.)


summary(meta_sub$Power..R.)



