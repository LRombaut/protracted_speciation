rm(list=ls())
library(expm)
library(ggplot2)

#define Matrix 1
mu.g <- 0.3
mu.i <- 0.3
b <- 1
lambda <- 0.2
lambda+mu.i <= 1
r1 <- c(-mu.g,lambda)
r2 <- c(b,-(mu.i+lambda))
M1 <- matrix(c(r1,r2),nrow=2,ncol=2,byrow = T)

#define Matrix 2
mu.g <- 0.1
mu.i <- 0.3
b <- 0.1
lambda <- 0.3
lambda+mu.i <= 1
r1 <- c(-mu.g,lambda)
r2 <- c(b,-(mu.i+lambda))
M2 <- matrix(c(r1,r2),nrow=2,ncol=2,byrow = T)

#simulate diversification
t <- seq(0,10,0.01)
n1 <- c(1,0)
N1 <- t(sapply(t,function(t) expm(M1*t)%*%n1))
N1 <- log(N1[,1])
n2 <- c(1,1)
N2 <- t(sapply(t,function(t) expm(M2*t)%*%n2))
N2 <- log(N2[,1])

par(mar=c(5,5,5,5))
plot(t,N1,type='l',col='red',lwd=3,ylim=c(-0.3,0.2),
     cex.axis=1.5,cex.lab=1.5,
     xlab=substitute(paste(bold('relative time'))),
     ylab=substitute(paste(bold('log(relative number of good species G)')))
)
lines(t,N2,col='blue',lwd=3)












