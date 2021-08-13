#weibull MLE with censored data

 library(MASS)
 library(survival)

 

data = c()

#indicator function, 1=censored, 0=not censored
data.cens = c(rep(1,41),rep(0,7)) 
      
surv = survreg(Surv(data, data.cens) ~ 1,dist="weibull")
summary(surv)

gamma.hat<-1/scale
alpha.hat<-e^intercept

#estimate parameters without censoring to compare
fitdistr(data,"weibull")
