
y = c(22, 20, 25, 17,26, 22, 27, 21,16, 20, 14, 18,
      20, 25, 26, 21,28, 29, 23, 24,22, 15, 19, 16 )

S1 = rep("S1",4)
S2 = rep("S2",4)
B1 = rep("B1",4)
B2 = rep("B2",4)
R1 = rep("R1",4)
R2 = rep("R2",4)
TYPE = c(S1,S2,B1,B2,R1,R2)
BRAND = as.factor(TYPE)

Brandmodel = lm(y ~ BRAND)
summary(Brandmodel)
AOV = aov(Brandmodel)
summary(AOV)

Compare = TukeyHSD(AOV,"BRAND",ordered=FALSE,.95)
Compare
plot(Compare)

plot(TukeyHSD(AOV,"BRAND",ordered=FALSE,.95))


