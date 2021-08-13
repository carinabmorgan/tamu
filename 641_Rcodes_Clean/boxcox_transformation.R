x <- c()
n <- length(x)
y <- abs(x)
ly <- log(y)
s <- sum(ly)
yt0 <- log(x)
varyt0 <- var(yt0)
Lt0 <- -1*s - .5*n*(log(6.28*varyt0)+1)		#6.28=2*pi, stays constant
th <- 0
Lt <- 0
t <- -3.01
i <- 0
while(t < 3)
{t <- t+.001
i <- i+1
th[i] <- t
yt <- (x^t -1)/t
varyt <- var(yt)
Lt[i] <- (t-1)*s - .5*n*(log(6.28*varyt)+1)
if(abs(th[i])<1.0e-10)Lt[i]<-Lt0
if(abs(th[i])<1.0e-10)th[i]<-0
}
# The following outputs the values of the likelihood and theta and yields
# the value of theta where likelihood is a maximum
out <- cbind(th,Lt)
Ltmax<- max(Lt)
imax<- which(Lt==max(Lt))
thmax<- th[imax]

plot(th,Lt,lab=c(30,50,7),main="Box-Cox Transformations",
           xlab="Theta",
           ylab="Objective Function, Lt(Theta)")

#the following plots a 95\% c.i. for theta

cic <- Ltmax-.5*qchisq(.95,1)  

del<- .01
iLtci <- which(abs(Li-cic)<=del)
iLtciL<- min(iLtci)
iLtciU<- max(iLtci)
thLci<- th[iLtciL]
thUci<- th[iLtciU]
abline(h=cic)
abline(v=thLci)
abline(v=thUci)

#graphs: 
par(mfrow=c(2,2))

qqnorm(x,main="Normal Prob Plots of Data",
                 xlab="normal quantiles",ylab="sample quantiles",cex=.65)
qqline(x)
text(-2,200,"SW=.9288")
text(-2,190,"p-value=0")
y1<- log(x)
y2<- x^thmax
y3<- x^.5
s <- shapiro.test(x)
s1 <- shapiro.test(y1)
s2 <- shapiro.test(y2)
s3 <- shapiro.test(y3)
qqnorm(y2,main="Maximized Normal Prob Plots of Data",
                 xlab="normal quantiles",ylab="y^thmax",cex=.65)
          qqline(y2)
text(-2,10,"SW=")
text(-2,9.7,"p-value=")


qqnorm(y1,main="Normal Prob Plots of Data with Log(x)",
                 xlab="normal quantiles",ylab="Ln(x)",cex=.65)
          qqline(y1)
text(-2,5.0,"SW=")
text(-2,4.85,"p-value=")



qqnorm(y3,main="Normal Prob Plots of Data with SQRT(x)",
                 xlab="normal quantiles",ylab="x^.5",cex=.65)
          qqline(y3)
text(-2,25,"SW=")
text(-2,24,"p-value=")

