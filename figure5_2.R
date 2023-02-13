rm(list=ls())
library(viridis)
library(ggplot2)

b <- 2 #rate of speciation initiation
mu.g <- seq(0,1,0.01) # rate of extinction of good species 
mu.i <- seq(0,0.8,0.01) #rate of extinction of incipient species 
lambda <- 0.2 #rate of transition from stage 1 to 2

#sanity check
mu.i + lambda <= 1

t1 <- sort(rep(mu.i,length(mu.g)))
t2 <- rep(mu.g,length(mu.i))
tt <- as.data.frame(cbind(t1,t2))

leading.eigenvalues <- numeric()

for(i in 1:length(tt[,1])){
  mu.i.t <- tt[i,1]
  mu.g.t <- tt[i,2]
  
  #express the differential equations as a parameter matrix
  r1 <- c(-mu.g.t,lambda)
  r2 <- c(b,-(mu.i.t+lambda))
  M <- as.matrix(rbind(r1,r2))
  
  leading.eigenvalues[i] <- Re(eigen(M)$values[2])
}

tt <- cbind(tt,leading.eigenvalues)
colnames(tt) <- c('mu.i','mu.g','lead.eig')

ggplot(aes(x=mu.i,y=mu.g,z=lead.eig,colour=lead.eig),data=tt)+
  geom_point()+
  geom_contour(colour='white',size=0.2)+
  scale_color_viridis()+
  xlab('')+
  ylab('')+
  theme(text=element_text(size=16),aspect.ratio = 1)

