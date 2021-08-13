t=4
r=7
n=t*(r-1)
alpha=.05
df1=t-1
df2=n-t
L = seq(0,45,.05)
phi = sqrt(L/t)
P = 1-pf(qf(1-alpha,df1,df2),df1,df2,L)
output = cbind(L,P,phi)
plot(L,P,type="l",main="POWER OF F-TEST VS Noncentrality Parameter",
           xlab="Noncentrality Parameter", ylab="POWER OF F-TEST",
         lab=c(20,20,7),ylim=c(0,1))
