#### Calculater
#Arithmetic
2*10
## [1]  3
#Order of operations is preserved
1+5*10
## [1] 51
(1+5)*10
2^5
9^(1/2)
c(1,4,6,7)
c(1:5,10)
seq(1,10,by=2)
seq(0,50,length=11)
seq(1,50,length=11)
1:10
5:1
seq(0,100*pi,by=pi)


a<- 1:5
a <- 1:5
a
b<- seq(15,3,length=5)
b
c<- a*b
c

a
sum(a)
prod(a)
mean(a)
sd(a)
var(a)
min(a)
media(a)
median(a)
max(a)
range(a)
a<-seq(0,100,by=10)
a
a<- seq(0,100,by=10)
a
a[]
a[5]
a[c(2,4,6,8)]
a[0]
a[-c(2,4,6,8)]
a[c(1,1,1,6,6,9)]
a[c(1,2)] <- c(333,555)
a
(a>50)
which(a>50)
a[{a>50}]
a[(a>50)]
!(a>50)
a[!(a>50)]
a
a[a==50]
a[(a==55)]
a[(a!=50)]
a[(a>50)]
a[(a<50)]
a[(a<=50)]
(c(10,14,40,60,99)%in%a)
a
a[(a>=50)&(a<=90)]
a[(a<50)|(a>100)]
a[(a<50)|!(a>100)]
a[(a>=50)&!(a<=90)]


NA+8
3*NA
mean(c(1,2,NA))
mean(c(NA,1,2),na.rm=TRUE)
sum(c(NA,1,2),na.rm = TRUE)
sum(c(NA,1,2)) 
sum(c(NA,1,2),na.rm = TRUE)
a<-c(NA,1.5,NA)
a
a<-c(NA,1:5,NA)
a
is.na(a)
!is.na(a)
a[!is.na(a)]
a
a<-a[!is.na(a)]
a


y<- c(5,9,12,30,14,18,32,40)
mean(y)
var(y)
ad(y)
sd(y)
sort(Y)
sort(y)
median(y)
fivenum(y)
summary(y)
fivenum(y)[c(2.4)]
fivenum(y)[4]-fivenum(y)[2]
diff(fivenum(y)[c(2,4)])
y2<- sort(y)[-length(y)]
y2
median(y2)
fivenum(y2)
diff(fivenum(y2)[c(2,4)])
bb<-c(141,148,132,138,154,142,150,146,155,158,150,140,147,148,144,150,149,145)
sort(bb)
length(bb)
summary(bb)
fivenum(bb)
range(bb)
diff(range(bb))
mean(bb)
sd(bb)
se<-sd(bb)/sqrt(length(bb))
se
par(mfrow=c(3,1))
stripchart(bb,main="Modern Englishman",xlab="head breadth(mm")
stripchart(bb,method="stack",cex=2,main="larger points(cex=2),method is stack")
stripchart(bb,method="jitter",cex=2,frame.plot=FALSE,main="no frame.method isjitter")


library(ggplot2)
hb_df<-data.frame(bb)
p<-ggplot(hb_df,aes(x=bb))
p<- p +geom_histogram(binwidth =5)
p<- p +labs(title="Modern Englishman head breadth")
print(p)
library(ggplot2)
hb_df<- data.frame(bb)
p1<-ggplot(hb_df,aes(x=bb))
p1<-p1+geom_dotplot(binwidth=2)
p1<-p1+labs(title="Modern Englishman head breadth")
p1<-p1+xlab("head breadth (mm)")

p2<- ggplot(hb_df,aes(x=bb))
p2<-p2 + geom_dotplot(binwidth=2,stackdir = "center")
p2<- p2 + labs(title = "Modern English head breadth,stackdir=center")
p2<- p2+xlab("head breadth(mm)")

p3<- ggplot(hb_df,aes(x=bb))
p3<-p3 + geom_dotplot(binwidth=2,stackdir = "centerwhole")
p3<- p3 + labs(title = "Modern English head breadth,stackdir=centerwhole")
p3<- p3+xlab("head breadth(mm)")


library(gridExtra)
grid.arrange(p1,p2,p3,ncol=1)

par(mfrow=c(1,3))
hist(bb,main="Modern Englishman",xlab="head breadth(mm)")
hist(bb,breaks=15,main="Histogram,15 breaks")
hist(bb,breaks=8,freq=FALSE,main="Histogram,density")

library(ggplot2)
hb_df<-data.frame(bb)
p<-ggplot(hb_df,aes(x=bb))
p<- p +geom_histogram(binwidth =5)
p<- p +labs(title="Modern Englishman head breadth")
print(p)


stem(bb)
stem(bb,scale = 2)
stem(bb,scale = 5)

fivenum(bb)
par(mfrow=c(1,1))
boxplot(bb,horizontal = TRUE,main="Modern English",xlab="head breadth(mm)")

library(ggplot2)
hb_df<- data.frame(bb)
p<- ggplot(hb_df,aes(x="bb",y=bb))
p<- p + geom_boxplot()
p<- p + coord_flip()
p<- p + labs(title = "Modern Englishman head breadth")
print(p)

par(mfrow=c(3,1))
hist(bb,freq = FALSE,main = "Histogram with kernel density plot,Modern Englishman")
points(density(bb),type = "1")
rug(bb)

library(vioplot)
install.packages(vioplot)
install.packages(vioplot)
vioplot(bb,horizontal=TRUE,col="gray")
title("Violin plot ,Modern Englishman")
boxplot(bb,horizontal = TRUE,main="Boxplot,Modern Englishman",xlab="head breadth")

income<- c(7,1110,7,5,8,12,0,5,2,2,46,7)
income<-sort(income,decreasing=TRUE)
income
summary(income)
stem(income)
income2<- income[-c(1,2)]
income2
summary(income2)
stem(income2)
stem(income2,scale = 2)

par(mfrow=c(1,3))
boxplot(income,main = "Income")
boxplot(income[-1],main="(remove largest)")
boxplot(income2,main = "(remove 2 largest)")
x1<- rnorm(250,mean = 100,sd=15)
par(mfrow=c(3,1))
hist(x1,freq = FALSE,breaks = 20)
points(density(x1),type = "1")
rug(x1)

library(vioplot)
vioplot(x1,horizontal=TRUE,col="gray")
boxplot(x1,horizontal=TRUE)
x1_df<-data.frame(x1)
p1 <- ggplot(x1_df, aes(x =x1))
p1 <-p1+geom_histogram(aes (y=..density..),binwidth=5,colour="black", fill="white")
p1<-p1+geom_density(alpha=0.1,fill="#FF6666")
p1 <-p1+geom_point(aes (y=-0.001),position=position_jitter(heigh=0.0005),alpha=1/5)
p2<-ggplot(x1_df, aes(x="x1",y=x1))
p2<-p2+ geom_violin(fill ="gray50")
p2<- p2+geom_boxplot(width = 0.2, alpha=3/4)
p2<- p2+ coord_flip()
P3<- ggplot(x1_df, aes(x="x1",y=x1))
p3<-p3+ geom_boxplot()
p3<-p3+coord_flip()
library(gridExtra)
grid.arrange(p1, p2, p3, ncol=1)
library(moments)
summary(x1)
sd(x1)
skewness(x1)
kurtosis(x1)
stem(x1)

x2.temp <- rnorm(250, mean =0, sd=1)
x2<-sign(x2.temp)*x2.temp-2*15+100
par(mfrow=c(3,1))
hist(x2,freq=FALSE,breaks=20)
points(density(x2),type="l")
rug(x2)
library(vioplot)
vioplot(x2,horizontal=TRUE,col="gray")
boxplot(x2,horizontal=TRUE)
x2_df<-data.frame(x2)
p1<-ggplot(x2_df,aes(x=x2))
p1<-p1+ geom_histogram(aes (y=..density..),binwidth=5,colour="black", fill="white")
p1 <-p1+geom_density(alpha=0.1, fill="#FF6666")
p1 <- p1+geom_point(aes(y=-0.001), position=position_jitter(height =0.0005),alpha=1/5)
p2<-ggplot(x2_df, aes(x ="x2", y=x2))
p2<-p2+geom_violin(fill ="gray50")
p2<-p2+geom_boxplot(width =0.2,alpha =3/4)
p2<- p2+ coord_flip()
p3<- ggplot(x2_df, aes(x ="x2",y=x2))
p3<- p3+ geom_boxplot()
p3<-p3+ coord_flip()
library (gridExtra)
grid.arrange(p1, p2, p3, ncol=1)
summary(x2)
sd(x2)
skewness(x2)
kurtosis(x2)
stem(x2)


x3<- runif(250,min=50,max=150)
par(mfrow=c(3,1))
hist(x3,freq=FALSE,breaks=20)
points(density(x3),type="l")
rug(x3)
library(vioplot)
vioplot(x3,horizontal=TRUE,col="gray")
boxplot(x3,horizontal=TRUE)
x3_df<- data.frame(x3)
p1 <- ggplot(x3_df, aes(x=x3))
p1 <- p1+geom_histogram(aes(y=..density..),binwidth=5,colour="black", fill="white")
P1 <- p1+geom_density(alpha=0.1, fill="#FF6666")
p1 <-p1+geom_point (aes(y =-0.001),position=position_jitter(height = 0.0005),alpha=1/5)
p2 <- ggplot(x3_df, aes(x="x3",y= x3))
p2<-p2+geom_violin(fill="gray50")
p2<-p2+ geom_boxplot(width=0.2, alpha =3/4)
p2<- p2+ coord_flip()
p3<- ggplot(x3_df, aes(x ="x3",y =x3))
p3<- p3+ geom_boxplot()
p3<- p3+coord_flip()

library (gridExtra)
grid.arrange(p1, p2, p3, ncol=1)
summary(x3)
sd(x3)
skewness(x3)
kurtosis(x3)
stem(x3)

x4<- rexp(250,rate=1)
par(mfrow = c(3,1))
hist(x4,freq=FALSE,breaks=20)
points(density(x4),type="1")
points(density(x4),type="l")
rug(x4)

library(vioplot)
vioplot(x4,horizontal=TRUE,col="gray")

boxplot(x4,horizontal=TRUE)

x4_df <- data.frame(x4)
p1<- ggplot(x4_df, aes(x= x4))
p1 <-p1+geom_histogram(aes(y=..density..),binwidth=0.5, colour="black", fill="white")
p1 <- p1+geom_density(alpha=0.1, fill="#FF6666")
p1 <- p1+geom_point(aes(y =-0.001),position=position_jitter(height =0.0005),alpha=1/5)
p2 <- ggplot(x4_df, aes(x="x4",y= x4))
p2<-p2+geom_violin(fill="gray50")
p2<-p2+ geom_boxplot(width=0.2, alpha =3/4)
p2<- p2+ coord_flip()
p3<- ggplot(x4_df, aes(x ="x4",y =x4))
p3<- p3+ geom_boxplot()
p3<- p3+coord_flip()
library (gridExtra)
grid.arrange(p1, p2, p3, ncol=1)
summary(x4)
sd(x4)
skewness(x4)
kurtosis(x4)
stem(x4)         

x5 <-15- rexp(250, rate = 0.5)
par(mfrow=c(3,1))
hist(x5, freq =FALSE, breaks= 20)
points(density(x5), type ="l")
rug(x5)
library(vioplot)
vioplot(x5, horizontal=TRUE, col="gray")
boxplot(x5, horizontal=TRUE)
x5_df <- data.frame(x5)
p1<- ggplot(x5_df, aes(x= x5))
p1 <-p1+geom_histogram(aes(y=..density..),binwidth=0.5, colour="black", fill="white")
p1 <- p1+geom_density(alpha=0.1, fill="#FF6666")
p1 <- p1+geom_point(aes(y =-0.001),position=position_jitter(height =0.0005),alpha=1/5)
p2 <- ggplot(x5_df, aes(x="x5",y= x5))
p2<-p2+geom_violin(fill="gray50")
p2<-p2+ geom_boxplot(width=0.2, alpha =3/4)
p2<- p2+ coord_flip()
p3<- ggplot(x5_df, aes(x ="x5",y =x5))
p3<- p3+ geom_boxplot()
p3<- p3+coord_flip()
library (gridExtra)
grid.arrange(p1, p2, p3, ncol=1)
summary(x5)
sd(x5)
skewness(x5)
kurtosis(x5)
stem(x5)  


x6 <- c(rnorm(150, mean = 100, sd=15), rnorm(150, mean=150, sd= 15))
par(mfrow=c(3,1))
hist(x6, freq =FALSE, breaks= 20)
points(density(x6), type ="l")
rug(x6)
library(vioplot)
vioplot(x6, horizontal=TRUE, col="gray")
boxplot(x6, horizontal=TRUE)
x6_df <- data.frame(x6)
p1<- ggplot(x6_df, aes(x= x6))
p1 <-p1+geom_histogram(aes(y=..density..),binwidth=0.5, colour="black", fill="white")
p1 <- p1+geom_density(alpha=0.1, fill="#FF6666")
p1 <- p1+geom_point(aes(y =-0.001),position=position_jitter(height =0.0005),alpha=1/5)
p2 <- ggplot(x6_df, aes(x="x6",y= x6))
p2<-p2+geom_violin(fill="gray50")
p2<-p2+ geom_boxplot(width=0.2, alpha =3/4)
p2<- p2+ coord_flip()
p3<- ggplot(x6_df, aes(x ="x6",y =x6))
p3<- p3+ geom_boxplot()
p3<- p3+coord_flip()
library (gridExtra)
grid.arrange(p1, p2, p3, ncol=1)
summary(x6)
sd(x6)
skewness(x6)
kurtosis(x6)
stem(x6)      










