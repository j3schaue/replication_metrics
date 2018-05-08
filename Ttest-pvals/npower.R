Npower<- function(d,power){
  
  #############################################
  # TAKES: d theta value
  #        tpower; power you want 
  # RETURNS: N need for a given d with a wanted power
  #############################################
  
  2/(-d/(qnorm(1-power)-1.96))^2
}


tabn<-matrix(data= NA, 3,3, dimnames = list(c("Power 40", "Power 60", "Power 80"), c("Theta 0.2", "Theta 0.5", "Theta 0.8")))

tabn[1,1]<-Npower(0.2, 0.4)
tabn[1,2]<-Npower(0.5, 0.4)
tabn[1,3]<-Npower(0.8, 0.4)

tabn[2,1]<-Npower(0.2, 0.6)
tabn[2,2]<-Npower(0.5, 0.6)
tabn[2,3]<-Npower(0.8, 0.6)


tabn[3,1]<-Npower(0.2, 0.8)
tabn[3,2]<-Npower(0.5, 0.8)
tabn[3,3]<-Npower(0.8, 0.8)

#############################
######## Table of n values to use
tabn

