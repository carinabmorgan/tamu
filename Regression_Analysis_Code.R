#REGRESSION MODEL CHECKING

#import data
  data<-read.csv()
  
#create model

    #linear model
    linearmod<-lm( , data=data)
    
    #logistic, poisson, or negative binomial model
    logmod<-glm( , data=data)

#look at diagnostic plots

par(mfrow=c(2,2))
plot(linearmod)

#identify leverage points
n<- #add number of data points
par(mfrow=c(1,1))
plot(hatvalues(linearmod), type='h')
points(hatvalues(linearmod), pch=21, col='purple', bg=2)
abline(h = (2*n)/length(data), col='yellow')
lev_points<-which(hatvalues(linearmod)>(2*n)/length(data))
lev_points 

#identify influential points
boxplot(cooks.distance(linearmod))

quantiles<-quantile(cooks.distance(linearmod))
iqr<-quantiles[4]-quantiles[2]

plot(cooks.distance(linearmod), type='h')
points(cooks.distance(linearmod), pch = 21, col='purple', bg=2)
abline(h=((1.5*iqr)+quantiles[4])) #shows influential points
abline(h=((3*iqr)+quantiles[4])) #shows highly influential points
infl<-which(cooks.distance(linearmod)>1.5*iqr + quantiles[4])
high_infl<-which(cooks.distance(linearmod)>3*iqr + quantiles[4])

#likelihood ratio test for coefficients

  #METHOD 1
    #null model
    null.mod<-lm()
    
    #alternative model
    alt.mod<-lm()
    
    df<-length(coef(alt.mod))-length(coef(null.mod))
    lambda<-(-2)*(as.numeric(logLik(null.mod))-as.numeric(logLik(alt.mod)))
    pval<-1-pchisq(lambda,df)
    print(pval)
  
  #METHOD 2
    anova(null.mod, alt.mod)
    
