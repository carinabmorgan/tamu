y= c()
mhat= median(y)
n = length(y)
M= 20000
d= numeric(M)
for(i in 1:M)
{ d[i]= median(sample(y,replace=TRUE))
}
hist(d)
bootmean= mean(d)
bootstd= sd(d)
bootquant= quantile(d)
#par(mfrow=c(2,2))

probs= seq(0,1,.01)
Qd= quantile(d,probs)

         boxplot(d,main="Sample Median",
                    ylab="Median",plot=T)

          plot(probs,Qd,type="l",ylab="Q(u) for Median",xlab="u",
          xlim=c(0,1),lab=c(10,11,7),main="Empirical Quantile for Sample Median",cex=.75)
 
 
    
          plot(density(d),type="l",
          xlab="Median",ylab="PDF of Sample Median",main=
        "Empirical pdf for Sample Median",cex=.75)


          qqnorm(d,main="Normal Prob Plot of Sample Median",
                 xlab="normal quantiles",ylab="Sample Medians",
                 lab=c(7,7,7),cex=.75)
          qqline(d)


#alternative method
library(boot)
y<-c()
B<-10000
boot_med<-function(y,indices) {
    y<-y[indices]
    median(y)
}
boot_med_out<-boot(y, boot_med, B)
boot.ci(boot_med_out, type=c('norm','perc','bca'))

plot(boot_med_out)