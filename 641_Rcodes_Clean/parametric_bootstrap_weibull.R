x =  c()
n =  length(x)
x  =  sort(x)
weib  =  log(x)
y = -log(x)
y = sort(y)
n  =  length(weib)
i =  1:n
ui =  (i-.5)/n
QW =  log(-log(1-ui))

plot(QW,weib,abline(lm(weib~QW)),
       main="Weibull Reference Plot",cex=.75,lab=c(7,11,7),
       xlab="Q(u) = log(-log(1-u))",
       ylab="log(W(i))" )
legend(-3.5,5.0,"y = 4.388+.4207 Q(u)")
legend(-3.5,4.7,"ADM = .3413, pvalue > .25")

library(MASS)
fitdistr(x,"weibull")

gamma = fitdistr(x,"weibull")$estimate[1]
alpha = fitdistr(x,"weibull")$estimate[2]
a = -log(alpha) 
b = 1/gamma
z   = exp(-exp(-(y-a)/b))  
A1i = (2*i-1)*log(z)
A2i = (2*n+1-2*i)*log(1-z)
s1  = sum(A1i)
s2  = sum(A2i)

AD  = -n-(1/n)*(s1+s2)
ADM  = AD*(1+.2/sqrt(n))
ADM


B  =  10000
W  =  matrix(0,B,n)
A =  numeric(B)
A =  rep(0,B)
G =  numeric(B)
G =  rep(0,B)
S = numeric(B)
S = rep(0,B)
{
for (i in 1:B) 
W[i,]  =  rweibull(n,gamma,alpha)
}

{
for (i in 1:B) 
G[i] = fitdistr(W[i,],"weibull")$estimate[1]
}
{
for (i in 1:B) 
A[i] = fitdistr(W[i,],"weibull")$estimate[2]
}
{
for (i in 1:B) 
S[i] = exp(-(20/A[i])^G[i])
}
summary(S)
sd(S)


boxplot(S,ylab="S(20)",main="Boxplot of 1000 values of S(20)")
out=c(mean(G),sd(G),mean(A),sd(A))
out

