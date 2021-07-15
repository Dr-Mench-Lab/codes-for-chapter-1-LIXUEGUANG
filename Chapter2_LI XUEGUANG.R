demo.clt.unif <- function(N,n)
  {
  sam <- matrix(runif(N*n,0,1),ncol=N)
  sam.mean <- colMeans(sam)
  sam.se <- sd(sam.mean)
  true.se<-sqrt((1/12)/n)
  hist(sam.mean,freq = FALSE,breaks = 25, main = paste( "True SEM =", round(true.se,4), ",Est SEM = ", round(sam.se,4)), xlab = paste( "n =", n))
  points(density(sam.mean), type = "l")
  x<- seq(0,1,length = 1000)
  points(x, dnorm(x,mean = 0.5,sd = true.se),type = "l", lwd = 2,col = "red")
  rug(sam.mean)
  
  }
par(mfrow=c(2,2));
demo.clt.unif(10000, 1);
demo.clt.unif(10000, 2);
demo.clt.unif(10000, 6);
demo.clt.unif(10000, 12);


demo.clt.exp <- function(N, n) {
  sam <- matrix(rexp (N*n,1) , ncol=N) ;
  sam.mean <- colMeans(sam)
  sam.se <- sd(sam.mean)
  true.se <- sqrt(1/n)
  hist(sam.mean,freq = FALSE,breaks = 25, 
       main = paste( "True SEM =", 
       round(true.se,4),"，Est SEM = ", 
       round( sam.se,4)), 
       xlab = paste( "n =", n))
  points(density(sam.mean), type = "l")
  x<- seq(0, 5, length = 1000)
  points(x, dnorm(x,mean = 1,sd = true.se),type = "l", lwd = 2,col = "red")
  rug(sam.mean)
}
par(mfrow=c(2,2));
demo.clt.exp(10000, 1);
demo.clt.exp(10000, 6);
demo.clt.exp(10000, 30);
demo.clt.exp(10000, 100);


x<- seq(-8,8, length = 1000)
par(mfrow=c(1,1))
plot(x, dnorm(x), type = "l",lwd = 2, col = "red", main = "Normal (red) vs t-dist with df=1，2，6，12，30，100")
points(x, dt(x,1), type = "l")
points(x, dt (x,2), type = "l")
points(x, dt(x,6), type = "l")
points(x, dt(x,12), type = "l")
points(x, dt(x,30), type = "l")
points(x, dt(x, 100), type = "l")


library(TeachingDemos)
ci.examp(mean.sim = 10, sd = 2, n = 25, reps = 100,conf.level= 0.95,method = "t")













