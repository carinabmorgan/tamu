install.packages("lsmeans")
install.packages("ggplot2")
library(lsmeans)
library(ggplot2)
install.packages("gmodels")
library("gmodels")

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
lsmeans(Brandmodel,"BRAND")


BrandContr1 = matrix(c( -1, -1, -1, -1, 2, 2,
                    1,  1, -1, -1, 0, 0,
                    0,  0,  0,  0, 1,-1, 
                    1, -1,  0,  0, 0, 0,
                    0,  0,  1, -1 ,0, 0), 6, 5)

BrandContr1 = t(BrandContr1)

rownames(BrandContr1) = c("TERRAIN VS OTHERS", "BIAS VS RADIAL", "WITHIN TERRAIN", "WITHIN BIAS", "WITHIN RADIAL")
            
contr2 = matrix(c(-1, -1, 0,  0, 1, 1, 
                   0, 0, -1, -1, 1, 1), 6, 2 )

BrandContr2 = t(contr2)
rownames(BrandContr2) = c("TERRAIN VS BIAS", "TERRAIN VS RADIAL")

options(digits = 3)

fit.contrast(Brandmodel, "BRAND", BrandContr1)

fit.contrast(Brandmodel, "BRAND", BrandContr2)



