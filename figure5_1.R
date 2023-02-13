rm(list=ls())

b <- 2 #rate of speciation initiation
mu.g <- 0.1 # rate of extinction of good species 
mu.i <- 0.2 #rate of extinction of incipient species
lambda <- seq(0,0.8,0.01) #rate of transition from incipient to good

#sanity check
mu.i + lambda <= 1

leading.eigenvalues <- numeric() 

for(i in 1:length(lambda)){
  
  lambda.t <- lambda[i]
  #express the differential equations as a parameter matrix
  r1 <- c(-mu.g,lambda.t)
  r2 <- c(b,-(mu.i+lambda.t))
  M <- as.matrix(rbind(r1,r2))
  
  leading.eigenvalues[i] <- eigen(M)$values[2]
  
}


plot(lambda,leading.eigenvalues,type='l',xlab='',ylab='')






