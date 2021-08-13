
x = c()

n = length(x)
x = sort(x)
i = seq(1:n)
u = (i-.5)/n
z = qnorm(u)


plot(z,x,datax=F,plot=T,xlab="Normal Quantile",ylab="Empirical Quantile",
       lab=c(7,8,7),
       main="Normal Reference Distribution Plot",
       cex=.95)
abline(lm(x~z))
shapiro.test(x)
text(-1.5,540,"SW=.393")
text(-1.5,500,"p-value=.0000000038")
m = mean(x)
SD = sd(x)

sigma0 = 100

TSD = (n-1)*SD^2/sigma0^2



B  =  9999
PV =  numeric(B)
PV =  rep(0,B)
TS =  numeric(B)
TS =  rep(0,B)
S =  numeric(B)
S =  rep(0,B)
Sy =  numeric(B)
Sy =  rep(0,B)


{
for (i in 1:B) 
S[i]  =  sd(sample(x,replace=T))
}

PV = (n-1)*S^2/SD^2
PV =  sort(PV)

#standardize the data so that the null hypothesis is true

y = x*sigma0/sd(x)

{
for (i in 1:B) 
Sy[i]  =  sd(sample(y,n,replace=T))
}

TS = (n-1)*Sy^2/sigma0^2 

SIMpvalue = sum(TS>TSD)/B

NORMpvalue = 1-pchisq(TSD,n-1)

LPV  =  PV[250]
UPV  =  PV[9750]
LPV2 = PV[9500]

SIMci = c(sqrt((n-1)*SD^2/UPV), sqrt((n-1)*SD^2/LPV))

SIMLcb = sqrt((n-1)*SD^2/LPV2)

NORMci = c(sqrt((n-1)*SD^2/qchisq(.975,n-1)), sqrt((n-1)*SD^2/qchisq(.025,n-1)))

LTS = TS[250]
UTS = TS[9750]

LN = qchisq(.025,n-1)
UN = qchisq(.975,n-1)

LEVEL = c(.025,.975,.025,.975)

PERCENTILES = c(LN,UN,LTS,UTS)

PVALUES = c(NORMpvalue,SIMpvalue)

compare = cbind(LEVEL,PERCENTILES,PVALUES)

SD

SIMci

NORMci

SIMpvalue

NORMpvalue

SIMLcb


