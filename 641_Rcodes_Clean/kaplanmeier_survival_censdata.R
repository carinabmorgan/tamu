
#Kaplan-Meier Estimate of Survival Function w/ Graph 
 library(MASS)
 library(survival)
 

data = c()

#indicator fun - 1 means censored, 0 means not censored
data.cens = c(rep(1,41),rep(0,7))

Surv(data, data.cens)

surv <- survfit(Surv(data, data.cens) ~ 0,conf.type="log-log")
summary(surv)
print(surv,print.rmean=TRUE)

plot(surv,conf.int=F,log=FALSE,ylim=c(0,1),
main="Kaplan-Meier Estimator of Survival Function",xlab="", 
ylab="Survival Function")




