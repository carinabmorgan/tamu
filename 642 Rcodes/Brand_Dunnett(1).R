library(lsmeans)

y = c(22, 20, 25, 17,26, 22, 27, 21,16, 20, 14, 18,
      20, 25, 26, 21,28, 29, 23, 24,22, 15, 19, 16 )

#Need to designate the Control Treatment first alphabetically. 
#Change S1 to A1

A1 = rep("A1",4)
S2 = rep("S2",4)
B1 = rep("B1",4)
B2 = rep("B2",4)
R1 = rep("R1",4)
R2 = rep("R2",4)
TYPE = c(A1,S2,B1,B2,R1,R2)
BRAND = as.factor(TYPE)

Brandmodel = lm(y ~ BRAND)
summary(Brandmodel)
AOV = aov(Brandmodel)
lsmeans(AOV,~BRAND)

lsmBrand = lsmeans(AOV, ~ BRAND)
levels(BRAND)
summary(contrast(lsmBrand, method="trt.vs.ctrl", adjust = "mvt", ref=1),
        infer=c(T,T), level = 0.95, side = "<")



