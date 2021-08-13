#kaplan-meier estimate comparing 2 groups survival

library(survival)

G1<-c()
G2<-c()
T<-c(G1,G2)
 
#censoring indicator function: 1's for G1, 0's for G1, 1's for G2, 0's for G2
ST = c(rep(1,17),rep(0,2),rep(1,19),rep(0,2))

#indicator for which group 
G = c(rep(1,length(G1)),rep(2,length(G2)))

out = cbind(T,ST,G)

Surv(T, ST)

surv <- survfit(Surv(T, ST) ~ G)
summary(surv)
print(surv, print.rmean=TRUE,rmean="individual")

par(lab=c(15,20,4))
plot(surv,ylab="Survival Function",xlab="Time to Death",mark.time=TRUE,
main="Estimated S(t)",lty=1:2 )
legend(25,.8,c("Group 1","Group 2"),lty=1:2,lwd=2)

