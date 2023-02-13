
rm(list=ls())

b <- seq(0,5,0.01) #rate of speciation initiation
mu.g <- 0.1 # rate of extinction of good species 
mu.i1 <- 0.2 #rate of extinction of incipient species in stage 1
mu.i2 <- 0.15 #rate of extinction of incipient species in stage 2
lambda1 <- 0.3 #rate of transition from stage 1 to 2
lambda2 <- 0.3 #rate of transition from stage 2 to good species

#sanity check
mu.i1 + lambda1 <= 1
mu.i2 + lambda2 <= 1

leading.eigenvalues <- numeric() 
  
for(i in 1:length(b)){
  
  b.t <- b[i]
  #express the differential equations as a parameter matrix
  r1 <- c(-mu.g,lambda2,0)
  r2 <- c(0,-(mu.i2+lambda2),lambda1)
  r3 <- c(b.t,0,-(mu.i1+lambda1))
  M <- as.matrix(rbind(r1,r2,r3))
  
  leading.eigenvalues[i] <- Re(eigen(M)$values[3])
    
}


plot(b,leading.eigenvalues,type='l',xlab='',ylab='')



