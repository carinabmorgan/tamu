library(stdReg) #standardized regression, used for causal effect analysis
library(AF)
data(rott2)

#fit coxPH model
fit<-coxph(Surv(rf, rfi) ~ no.chemo + age + size + factor(grade) + 
             I(exp(-0.12*nodes)) + pr + er, data=rott2, method='breslow')
            #use breslow approx to account for ties

summary(fit)

#estimate cumulative hazard function, H(t) hat, the predicted survival curve for a Cox model
out<-basehaz(fit, centered=FALSE)
out[90,] #at time t=7.39, the hazard is 0.140

#calculate causal survival probability at t=7.39
fit.std<-stdCoxph(fit=fit, data=rott2, X='no.chemo', t=out[90,2], x=c(0,1))
        # fit = coxph model, X = confounding (z) variable, x = possible outcomes
fit.std$est 
        #the first number is P(Y(0) > 7.39), the chemo group  and the second is P(Y(1)>7.39), the non chemo group estimations
fit.std$vcov

#estimated effect
est<-fit.std$est[1]-fit.std$est[2]   #effect is small at .01
est.error<-sqrt(fit.std$vcov[[1]][1,1] + 
                  fit.std$vcov[[1]][2,2] - 
                  2*fit.std$vcov[[1]][1,2])

#95% CI for estimated effect:
lb<-est-1.96*est.error
ub<-est+1.96*est.error
ci<-cbind(lb,est,ub)
print(ci)

#test significance of estimated effect
#H0: est = 0 vs Ha: est not equal 0
TS<-est/est.error

#check that TS is at least 3 standard deviations away from 0 for significance
3*est.error
TS-3*est.error     #well above 0, so our effect is statistically significant, though practically small

#plot the causal effect for different values of t

#first obtain an estiamte fit.std for a series of k times
fit.std2<-
myest<-fit.std2$est[,1]-fit.std2$est[,2]
mysd<-NULL
for(k in 1:51) {
  mysd[k]<-sqrt(fit.std2$vcov[[k]][1,1] + 
                  fit.std2$vcov[[k]][2,2] - 
                  2*fit.std2$vcov[[k]][1,2])
}
plot(10:60, est, type='l', ylim=c(0,0.15), ylab='ATE', xlab='Time', lwd=2)
par(new=T)
plot(10:60, est-1.96*mysd, type='l', ylim=c(0,0.15), col='purple', axes=F, 
     ylab='', xlab='', lwd=2)
par(new=T)
plot(10:60, est+1.96*mysd, type='l', ylim=c(0,0.15), col='purple', axes=F, 
     ylab='', xlab='', lwd=2)

#analyze causal ratio at a specific time t: causal effect notes, slide 37
  #remember if the ratio = 1, no causal effect
myest2<-fit.std2[,1]/fit.std2[,2]
n<-length(myest2)
mysd2<-NULL
for (k in 1:n){
  mysd[k]<- mysd[k]<-sqrt(fit.std2$vcov[[k]][1,1]/(fit.std2$est[k,1])^2 + 
                            fit.std2$vcov[[k]][2,2]/(fit.std2$est[k,2])^2 - 
                            2*fit.std2$vcov[[k]][1,2]/(fit.std2$est[k,1]*fit.std2$est[k,2])
                          )
                }

plot(10:60, myest2, type='l', ylab='ATE', xlim='Time', lwd=2)
par(new=T)
plot(10:60, exp(log(myest2)-1.96*mysd2), type='l', axes=F, 
     ylab='', xlim='', col='purple',lwd=2)
par(new=T)
plot(10:60, exp(log(myest2)+1.96*mysd2), type='l', axes=F, 
     ylab='', xlim='', col='purple',lwd=2)


#NNT: how man subjects should be treated to avoid one unfavorable outcome on avg at a fixed time point





#