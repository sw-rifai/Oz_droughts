library(tidyverse); library(Rssa)
# Simulate two periodic time series with independent noise
# Can SSA recover the time series?
nobs <- 1000
x1 <- 2*sin(seq(1,8*pi,length.out = nobs))+0.01*c(1:nobs)
x2 <- sin(seq(-16,16*pi,length.out = nobs))
eps <- rnorm(nobs,mean=0,sd=0.15)
x1 <- ts(x1, start=1,end=10,frequency = nobs/10)
x2 <- ts(x2, start=1,end=10,frequency = nobs/10)
eps <- ts(eps, start=1,end=10,frequency = nobs/10)
x <- x1+x2+eps


# Plot simulated time series ----------------------------------------------
plot(x, type='l')
lines(x1,col='blue')
lines(x2,col='red')

L <- 1000/4
s <- ssa(x, L=L)
summary(s)
plot(s)

nsigma(s)
nu(s)
nv(s)
nspecial(s)

reconstruct(s,groups=list(c(1),c(2),c(3),c(4),c(5),c(6),c(7))) %>% 
  plot(., ylim=c(-1,1))
reconstruct(s,groups=list(c(1,2),c(3,4))) %>% 
  plot(., ylim=c(-1,1))

# recover 1st sine wave
reconstruct(s, groups = list(c(1,2,3)))$F1 %>% plot(., ylim=c(0,10))
lines(x1,col='blue')

# recover 2nd sine wave
reconstruct(s, groups = list(c(4,5,6)))$F1 %>% plot(., ylim=c(-3,3))
lines(x2,col='red')


