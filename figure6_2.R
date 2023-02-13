rm(list=ls())
library(viridis)
library(ggplot2)

b <- 2 #rate of speciation initiation
mu.g <- 0.1 # rate of extinction of good species 
mu.i1 <- 0.2 #rate of extinction of incipient species in stage 1
mu.i2 <- 0.15 #rate of extinction of incipient species in stage 2
lambda1 <- seq(0,0.8,0.01) #rate of transition from stage 1 to 2
lambda2 <- seq(0,0.8,0.01) #rate of transition from stage 2 to good species

#sanity check
mu.i1 + lambda1 <= 1
mu.i2 + lambda2 <= 1

t1 <- sort(rep(lambda1,length(lambda2)))
t2 <- rep(lambda2,length(lambda1))
tt <- as.data.frame(cbind(t1,t2))

leading.eigenvalues <- numeric()

for(i in 1:length(tt[,1])){
  lambda1.t <- tt[i,1]
  lambda2.t <- tt[i,2]
  
  #express the differential equations as a parameter matrix
  r1 <- c(-mu.g,lambda2.t,0)
  r2 <- c(0,-(mu.i2+lambda2.t),lambda1.t)
  r3 <- c(b,0,-(mu.i1+lambda1.t))
  M <- as.matrix(rbind(r1,r2,r3))
  
  leading.eigenvalues[i] <- Re(eigen(M)$values[3])
}

tt <- cbind(tt,leading.eigenvalues)
colnames(tt) <- c('lambda1','lambda2','lead.eig')

ggplot(aes(x=lambda1,y=lambda2,z=lead.eig,colour=lead.eig),data=tt)+
  geom_point()+
  geom_contour(colour='white',size=0.2)+
  scale_color_viridis()+
  xlab('')+
  ylab('')+
  theme(text=element_text(size=16),aspect.ratio = 1)
















