#Reference plot where Fo completely specified (for any distribution)

y<-sort(c())

#Qo function will be different based on the distribution
u<-seq(1/2n, 1-1/2n, 1/n)

#gamma
alpha<-
beta<-
Qo<-qgamma(u,alpha,beta)

#normal
mu<-
sigma<-
Qo<-qnorm(u,mu,sigma)

plot(Qo,y,abline(c(0,1)),
       main="Reference Plot",cex=.75,lab=c(7,11,7),
       xlab="Qo(u)",
       ylab="Sample Quantile  Q(u)")
abline(h=0)

##################
#Reference plot for Fo where Fo ~ N(mu,sigma^2) and mu/sigma unknown

y<-c()
n<-length(y)
i<-seq(1,n,1)
u<-seq(1/(2*n),1-1/(2*n),1/n)
z<-qnorm(u,0,1)

plot(z,y, main='Normal Reference Plot', xlab='standard normal', ylab='sample Q')


##################
#Reference plot where Fo~Exp(b) w/ b unkown
y<-c()
n<-length(y)
i<-seq(1,n,1)
u<-(i-.5)/n
z<- -log(1-u)

plot(z,y, main='Reference Plot for Exponential', xlab='Qz for Exp', ylab='sample Q')

###############
#Reference Plot Where Fo~ Uniform, unspecified
y<-c()
n<-length(y)
i<-seq(1,n,1)
u<-(i-.5)/n

plot(u,y,main='Reference Plot for Uniform', xlab='Qz for Unif', ylab='Fo')

################
#Reference Plot where Fo~Weibull w/ alpha & gamma unknown
y<-c()
n<-length(y)
i<-seq(1,n,1)
u<-(i-.5)/n

Qz<-log(-log(1-u))
x<-log(y)

plot(Qz,x,main='Reference Plot for Weibull', xlab='Qz for log(Weibull)', ylab='sample Q for log(y)')


