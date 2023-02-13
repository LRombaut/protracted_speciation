rm(list=ls())
library(expm)
library(ggplot2)

mu.g <- 0.1
mu.i <- 0.3
b <- 2
lambda <- 0.3

lambda+mu.i <= 1

r1 <- c(-mu.g,lambda)
r2 <- c(b,-(mu.i+lambda))

M <- as.matrix(rbind(r1,r2))

eigens <- eigen(M)
evector1 <- eigens$vectors[,2]

t <- seq(0,4,0.01)
n1 <- c(1,1)
N1 <- t(sapply(t,function(t) expm(M*t)%*%n1))
N1 <- log(N1)
colnames(N1) <- c('good','incipient')
N1 <- as.data.frame(N1)

t <- seq(0,3,0.01)
n2 <- c(1,exp(1))
N2 <- t(sapply(t,function(t) expm(M*t)%*%n2))
N2 <- log(N2)
colnames(N2) <- c('good','incipient')
N2 <- as.data.frame(N2)

t <- seq(0,3,0.01)
n3 <- c(exp(0.5),1)
N3 <- t(sapply(t,function(t) expm(M*t)%*%n3))
N3 <- log(N3)
colnames(N3) <- c('good','incipient')
N3 <- as.data.frame(N3)

t <- seq(0,3,0.01)
n4 <- c(1,exp(1.5))
N4 <- t(sapply(t,function(t) expm(M*t)%*%n4))
N4 <- log(N4)
colnames(N4) <- c('good','incipient')
N4 <- as.data.frame(N4)

r1 <- log(evector1/evector1[1])
r2 <- log(evector1/evector1[1]*exp(1.2))

ggplot(N1,aes(x=good,y=incipient))+
  geom_line()+
  geom_line(data=N2)+
  geom_line(data=N3)+
  geom_line(data=N4)+
  geom_segment(aes(x=r1[1],y=r1[2],xend=r2[1],yend=r2[2]),col='blue',arrow=arrow())+
  xlim(0,1.25)+
  ylim(0,2)+
  xlab('log(relative number of good species)')+
  ylab('log(relative number of incipient species)')+
  theme_bw()+
  theme(text=element_text(size=15))




