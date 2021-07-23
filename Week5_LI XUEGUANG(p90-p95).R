library(pwr)
toco <- c(5.6,2.7,6.2,2.9,1.5,4.0,4.3,3.0,3.6,2.4,6.7,3.8)
?power.t.test
power.t.test(n = NULL, delta=1.00-0.54, sd= sd(toco),sig.level =0.05, power =0.50,type ="one.sample", alternative ="two.sided")
power.t.test(n= NULL, delta =0.75-0.54, sd=sd(toco),sig.level =0.05, power =0.50,type ="one.sample", alternative ="two.sided")
library(TeachingDemos)
?power.examp

tomato<-c(20.5,18.5,20.0,19.5,19.5,21.0,17.5,22.5,20.0,19.5,18.5,20.0,18.0,20.5)
par(mfrow=c(2, 1))
hist(tomato, freq= FALSE, breaks =6)
points(density(tomato), type ="l")
rug(tomato)


library(vioplot)
vioplot(tomato, horizontal=TRUE, col="gray")
stem(toco, scale=2)
qt(1-0.05/2, df=length(tomato)-1)
t.summary <-t.test(tomato, mu =20,alternative = "less")
t.summary
summary(tomato)

t.dist.pval(t.summary)

bs.one.samp.dist(tomato)

