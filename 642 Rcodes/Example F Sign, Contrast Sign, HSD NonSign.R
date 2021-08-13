install.packages("lsmeans")
install.packages("ggplot2")
install.packages("gmodels")

library(lsmeans)
library(ggplot2)
library("gmodels")



y1 = .78 + rnorm(21,0,.95)
y2 = rnorm(21,0,.95)
y3 = rnorm(21,0,.95)
y4 = rnorm(21,0,.95)

y3 = y3-.06

y = c(y1,y2,y3,y4)

S1 = rep("S1",21)
S2 = rep("S2",21)
S3 = rep("S3",21)
S4 = rep("S4",21)

TYPE = c(S1,S2,S3,S4)
TRT = as.factor(TYPE)
TRTmodel = lm(y ~ TRT)
summary(TRTmodel)
AOV = aov(TRTmodel)
summary(AOV)
lsmeans(TRTmodel,"TRT")


TRTContrast = c( 3, -1, -1 , -1)
fit.contrast(TRTmodel, "TRT", TRTContrast)


Compare = TukeyHSD(AOV,"TRT",ordered=FALSE,.95)
Compare


