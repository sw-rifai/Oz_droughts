library(tidyverse); library(Rssa)
nobs <- 100
x1 <- 0.5*sin(seq(1,8*pi,length.out = nobs))+0.01*c(1:nobs)
x2 <- sin(seq(-16,16*pi,length.out = nobs)+0.15*pi)
eps <- rnorm(nobs,mean=0,sd=0.15)
x1 <- ts(x1, start=1,end=10,frequency = nobs/10)
x2 <- ts(x2, start=1,end=10,frequency = nobs/10)
eps <- ts(eps, start=1,end=10,frequency = nobs/10)
x <- x1+x2+eps
x <- ts(x,start=c(1),end=c(10),frequency = nobs/10)

plot(x, type='l')
lines(x1,col='blue')
lines(x2,col='red')

s <- ssa(x, L=30)
reconstruct(s, groups = c(1)) %>% plot

reconstruct(s,groups=c(1))$F1 %>% plot
lines(x1,col='red')

reconstruct(s,groups=list(c(1),c(2))) %>% 
  plot(., ylim=c(-1,1))
lines(x2,col='red')


plot(s)
plot(s, type = "vectors")
plot(s, type = "series")
plot(s, type = "paired")

x
stats::decompose(x) %>% plot

