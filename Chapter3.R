english <- c(141, 148, 132, 138, 154, 142, 150, 146, 155, 158,
             150, 140, 147, 148, 144, 150, 149, 145)
celts <- c(133, 138, 130, 138, 134, 127, 128, 138, 136, 131,
           126, 120, 124, 132, 132, 125)
english
celts

HeadBreadth <- c(english, celts)
Group <- c(rep("English", length(english)), rep("Celts", length(celts)))
hb <- data.frame(HeadBreadth, Group)
hb

stripchart(HeadBreadth ~ Group, method = "stack", data = hb,
           main = "Head breadth comparison", xlab = "head breadth (mm)")

library(ggplot2)
p<- ggplot(hb,aes(x = HeadBreadth))
p<- p + geom_dotplot(binwidth = 2)
p<- p + facet_grid(Group ~ .)
p<- p + labs(title = "Head breadth comparison") + xlab("head breadth (mm)")
print(p)

boxplot(HeadBreadth ~ Group, method = "stack", data = hb,
        horizontal = TRUE,
        main = "Head breadth comparison", xlab = "head breadth (mm)")
p<- ggplot(hb, aes(x = Group, y = HeadBreadth))
p<- p+geom_boxplot( )
p<- p+stat_summary(fun.y = mean, geom = "point", shape = 3, size = 2 )
p<- p+coord_flip( )
p<- p+labs(title = "Head breadth comparison")
print(p)

par(mfcol=c(2,2))
hist(hb$HeadBreadth[(hb$Group == "Celts")],
     main = "Head breadth, Celts", xlab = "head breadth (mm)")
hist(hb$HeadBreadth[(hb$Group == "English")],
     main = "Head breadth, English", xlab = "head breadth (mm)")
hist(hb$HeadBreadth[(hb$Group == "Celts")], xlim = range(hb$HeadBreadth),
     main = "Head breadth, Celts", xlab = "head breadth (mm)")
hist(hb$HeadBreadth[(hb$Group == "English")],xlim = range(hb$HeadBreadth),
     main = "Head breadth, English", xlab = "head breadth (mm)")

p<- ggplot(hb, aes(x = HeadBreadth))
p<- p+geom_histogram(binwidth = 4 )
p<- p+facet_grid(Group ~ .)
p<- p+labs(title = "Head breadth comparison") + xlab("head breadth (mm)")
print(p)
p<- ggplot(hb, aes(x = HeadBreadth, fill=Group))
p<- p+geom_histogram(binwidth = 4, alpha = 0.5, position="identity")
p<- p+labs(title = "Head breadth comparison") + xlab("head breadth (mm)")
print(p)
p<- ggplot(hb, aes(x = HeadBreadth, fill=Group))
p<- p+geom_histogram(binwidth = 4, alpha = 1, position="dodge")
p<- p+labs(title = "Head breadth comparison") + xlab("head breadth (mm)")
print(p)

stem(english, scale = 2)
stem(celts, scale = 2)

summary(english)
summary(celts)
sd(english)
sd(celts)
IQR(english)
IQR(celts)
by(hb, Group, summary)

m1<-mean(celts)
s1<-sd(celts)
n1<-length(celts)
m2<-mean(english)
s2<-sd(english)
n2<-length(english)
c(m1,s1,n1)
c(m2,s2,n2)

sdpool<-sqrt(((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2))
sdpool
SEpool<-sdpool*sqrt(1/n1+1/n2)
SEpool
dfpool<-n1+n2-2
dfpool
t_pool<-(m1-m2)/SEpool
t_pool

SE_Sat<-sqrt(s1^2/n1+s2^2/n2)
SE_Sat
df_Sat <- (SE_Sat^2)^2/(s1^4/(n1^2*(n1-1))+s2^4/(n2^2*(n2-1)))
df_Sat
t_Sat<-(m1-m2)/SE_Sat
t_Sat

t.summary.eqvar<-t.test(celts, english, var.equal = TRUE)
t.summary.eqvar
t.summary.uneqvar<-t.test(HeadBreadth~Group, data = hb, var.equal = FALSE)
t.summary.uneqvar

bs.two.samp.diff.dist<-function(dat1, dat2, N = 1e4){
  n1<-length(dat1);
  n2<-length(dat2);
  sam1<-matrix(sample(dat1,size=N*n1,replace=TRUE),ncol=N);
  sam2<-matrix(sample(dat2,size=N*n2,replace=TRUE),ncol=N);
  sam1.mean<-colMeans(sam1);
  sam2.mean<-colMeans(sam2);
  diff.mean<-sam1.mean-sam2.mean;
  old.par<-par(no.readonly=TRUE)
  par(mfrow=c(3,1),mar=c(3,2,2,1),oma=c(1,1,1,1))
  hist(dat1,freq=FALSE,breaks=6
       ,main=paste("Sample 1","\n"
                   ,"n=",n1
                   ,",mean=",signif(mean(dat1),digits=5)
                   ,",sd=",signif(sd(dat1),digits=5))
       ,xlim=range(c(dat1,dat2)))
  points(density(dat1),type="l")
  rug(dat1)
  hist(dat2,freq=FALSE,breaks = 6
       ,main=paste("Sample 2","\n"
                   ,"n=",n2
                   ,",mean=",signif(mean(dat2),digits=5)
                   ,",sd=",signif(sd(dat2),digits=5))
       ,xlim=range(c(dat1,dat2)))
  points(density(dat2),type="l")
  rug(dat2)
  hist(diff.mean,freq=FALSE,breaks=25
       ,mean=paste("Bootstrap sampling distribution of the difference in means","\n"
                   ,"mean=",signif(mean(diff.mean),digits=5)
                   ,",se=",signif(sd(diff.mean),digits=5)))
  points(density(diff.mean),type="l")
  x<-seq(min(diff.mean),max(diff.mean),length=1000)
  points(x,dnorm(x,mean=mean(diff.mean),sd=sd(diff.mean))
         ,type="l",lwd=2,col="red")
  rug(diff.mean)
  par(old.par)
}

bs.two.samp.diff.dist(celts,english)

men <- c(217, 123, 80, 140, 115, 135, 59, 126, 70, 63,
         147, 122, 108, 70)
women <- c(84, 87, 77, 84, 73, 66, 70, 35, 77, 73,
           56, 112, 56, 84, 80, 101, 66, 84)
level <- c(men, women)
sex <- c(rep("men", length(men)), rep("women", length(women)))
andro<- data.frame(level, sex)
andro
by(andro, sex, summary)
c(sd(men), sd(women), IQR(men), IQR(women), length(men), length(women))
p<- ggplot(andro, aes(x = sex, y = level, fill=sex))
p<- p+geom_boxplot( )
p<- p+stat_summary(fun = mean, geom = "point", shape = 3, size = 2 )
p<- p+labs(title = "Androstenedione Levels in Diabetics")
print(p)
p<- ggplot(andro, aes(x = level, fill=sex))
p<- p+geom_histogram(binwidth = 20, alpha = 0.5, position="identity")
p<- p+labs(title = "Androstenedione Levels in Diabetics") 
print(p)

bs.two.samp.diff.dist(men, women)

t.summary <- t.test(level ~ sex, data = andro, var.equal = FALSE)
t.summary 
t.dist.pval(t.summary)


a <- c(0.7, -1.6, -0.2, -1.2, 0.1, 3.4, 3.7, 0.8, 0.0, 2.0)
b <- c(1.9, 0.8, 1.1, 0.1, -0.1, 4.4, 5.5, 1.6, 4.6, 3.0)
d <- b - a;
sleep <- data.frame(a, b, d)
sleep
p<- ggplot(sleep, aes(x = a, y = b))
p<- p+geom_abline(intercept=0, slope=1,alpha=0.2)
p<- p+geom_point( )
p<- p+coord_equal( )
p<- p+labs(title = "Sleep hours gained on two sleep remedies: a vs b") 
print(p)

p1 <- ggplot(sleep, aes(x=d))
p1 <- p1+scale_x_continuous(limits = c(-5,+5))
p1 <- p1+geom_vline(xintercept =0, colour="#BB0000", linetype="dashed")
p1 <- p1+geom_histogram(aes(y=..density..),binwidth = 1,
                        colour="black", fill="white")
p1 <- p1+geom_density(alpha=0.1, fill="#FF6666")
p1 <- p1+geom_point(aes(y=-0.01),position = position_jitter(height = 0.005),alpha=1/5)
p1 <- p1+labs(title="Difference of sleep hours gained: d=b-a")

p2 <- ggplot(sleep, aes(x="d", y=d))
p2 <- p2+scale_y_continuous(limits = c(-5,+5))
p2 <- p2+geom_hline(yintercept =0, colour="#BB0000", linetype="dashed")
p2 <- p2+geom_violin(fill="gray50", alpha=1/2)
p2 <- p2+geom_boxplot(width=0.2, alpha=3/4)
p2 <- p2+coord_flip()

p3 <- ggplot(sleep, aes(x="d", y=d))
p3 <- p3+scale_y_continuous(limits = c(-5,+5))
p3 <- p3+geom_hline(yintercept =0, colour="#BB0000", linetype="dashed")
p3 <- p3+geom_boxplot()
p3 <- p3+coord_flip()

library(gridExtra)
grid.arrange(p1,p2,p3,ncol=1)
bs.one.samp.dist(sleep$d)

t.summary <- t.test(sleep$d)
t.summary
t.dist.pval(t.summary)









