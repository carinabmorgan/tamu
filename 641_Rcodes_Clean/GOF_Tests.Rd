#GOF TESTS

###################3
#Chi-Sq GOF Test: discrete distribution/categorical data, completely specified
#check conditions: fixed number of trials, each trial is S/F, all trials independent

#choose k groups based on data
k<-seq(0,k,1)

#frequencies of observations
Oi<-c()
n<-sum(Oi)

#assign probabilities to each of the k groups
pi<-c()

#expected number of observations - all Ei must be > 0 and only 20 percent can be less than 5. If these don't hold, combine the k groups as needed.
Ei<-n*pi

#test statistic
Q<-sum((Oi=Ei)^2/Ei)

#if Q=0, then Oi=Ei (great fit), if Q is large, bad fit

#pvalue: high pval if Q=0 aka good fit, low pval if bad fit

pval<-pchisq(Q,k-1)


#Hosmer-Lemeshow Test: tests GOF in logistic regression models for categorical data. 
library(generalhoslem)

#response variable y
y<-c()

#predictor variables xi
x<-c()

#create model
mod<-glm(y ~ x, family=binomial(link='logit'), data=)

#run H-L test (H0: model does not fit the data, Ha: Model fits data)
logitgof(y, fitted(mod))

#######################
#Fitting Binomial (n,theta) Model - discrete, unspecified
n<-
N<-

#find MLE
thetahat<-

#find pi's
i<-seq(1,k,1)
pi<-combinations(n, i, v=1:n, set=TRUE, repeats.allowed=FALSE)*(thetahat^i)*((1-thetahat)^n-i)

#observed values
Oi<-c()

#expected values
Ei<-n*pi

#test statistic
Q<-sum(((Oi-Ei)^2)/Ei)

#df=k-1-1(the number of unknown parameters)

pval<-1-pchisq(Q,df)

#######################
#Fitting Poisson Model: discrete, unspecified

#data: 
X<-c()
N<-length(X)

#MLE of lambda is Xbar
lambda.hat<-mean(X)

#select k
k<-

i<-seq(1,k-1,1)

#calculate pi's
pi<-dpois(i,lambda.hat)

#to get the last pi, need to take 1-the sum of the other pi's
pk<-1-ppois(length(i),lambda.hat)

#combine p's:
pi<-c(pi,pk)

#observed values:
Oi<-c()

#expected values: 
Ei<-N*pi

#test statistic: 
Q<-sum((Oi-Ei)^2/Ei)

#df = k-1-1(the number of unknown parameters)

pval<-1-pchisq(Q,df)

##################
#Fitting normal distribution: KS, CvM, AD, shapiro-wilks

x = c()
n = length(x)
mu = 200 		#if unspecified, use xbar
sigma = 35 		#if unspecified, use sd(x)
x = sort(x)
z = pnorm(x,mu,sigma)   
#z computes F0(X(i)), this is a normal dist but could change based on what distribution you're trying to fit

i = seq(1,n,1)

# K-S Computations:

d1 = i/n - z
dp = max(d1)
d2 = z - (i - 1)/n
dm = max(d2)
ks = max(dp,dm) #regular KS
KS = ks*(sqrt(n)+.12+0.11/sqrt(n)) #modified KS
#reject normality at 0.05 level if KS > 1.358

# Cramer-von Mises Computations:

wi = (z-(2*i-1)/(2*n))^2
s = sum(wi)
cvm = s + 1/(12*n)
CvM = (cvm-.4/n+.6/n**2)*(1+1/n)

#reject normality at 0.05 level if CvM > 0.461



# Anderson-Darling Computations:

a1i = (2*i-1)*log(z)
a2i = (2*n+1-2*i)*log(1-z)
s1 = sum(a1i)
s2 = sum(a2i)
AD = -n-(1/n)*(s1+s2)

#reject normality at 0.05 level if AD > 2.492

data = cbind(z,d1,d2,wi,a1i,a2i)


#Shapriro-Wilks: Fitting normal distribution ONLY

#data: 
shapiro.test(x)

#another method for fitting normal: 
i<-seq(1,n,1)
u<-(i-.375)/(n+.25)
q<-qnorm(u)
r<-cor.test(q,x)
print(r)

####################
#Exponential Distribution: AD

#data: 
w  =  c()

n  =  length(w)
lam  =  mean(w)
w  =  sort(w)
z  =  1-exp(-w/lam)   #computes F0(X(i))
i  =  seq(1,n,1)

a1i  =  (2*i-1)*log(z)
a2i  =  (2*n+1-2*i)*log(1-z)
s1  =  sum(a1i)
s2  =  sum(a2i)
AD  =  -n-(1/n)*(s1+s2)		#AD measure
MAD  =  AD*(1+.6/n)			#modified AD
MAD

#probability plot: 
j  =  (i-.5)/n
quexp  =  -log(1-j)
plot(quexp,w,xlab="Exponential Quantiles",ylab="Sample Quantiles")
title("Exponential Probability Plot")
abline(lm(w~quexp))


###################
Weibull Distribution: AD

x = c()

n = length(x)
y = -log(x)
y = sort(y)
i= 1:n

library(MASS)
mle <- fitdistr(x,"weibull")
shape = mle$estimate[1]
scale = mle$estimate[2]
a = -log(scale) 
b = 1/shape
z   = exp(-exp(-(y-a)/b))  
A1i = (2*i-1)*log(z)
A2i = (2*n+1-2*i)*log(1-z)
s1  = sum(A1i)
s2  = sum(A2i)

AD  = -n-(1/n)*(s1+s2)
ADM  = AD*(1+.2/sqrt(n))
AD
ADM

#probability plot
n = length(y)
weib= log(x)
weib= sort(weib)
i= 1:n
ui= (i-.5)/n
QW= log(-log(1-ui))
plot(QW,weib,abline(lm(weib~QW)),
       main="Weibull Reference Plot",cex=.75,lab=c(7,11,7),
       xlab=expression(Q(u[i]) == log(-log(1-u[i]))),
       ylab=expression(y == log(W[(i)])))
legend(-3.5,5.0,expression(y == 4.388+.4207*Q(u)))
legend(-3.5,4.7,"ADM=.3413, p-value>.25")


