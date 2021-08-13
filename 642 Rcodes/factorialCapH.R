
y = c(5.2, 5.9, 6.3, 7.1, 7.3, 7.5, 7.6, 7.2, 7.4, 7.2, 7.5, 7.2,
      7.6, 7.0, 7.6, 7.5, 7.3, 7.1, 7.6, 7.4, 7.8, 7.4, 7.0, 6.9,
      6.4, 6.7, 6.1, 7.3, 7.5, 7.4, 7.2, 7.3, 7.1, 6.8, 6.6, 6.4)

Calcium =c(rep("100",12), rep("200",12), rep("300",12))

Ca = factor(Calcium)

ph = c(rep("4",3),rep("5",3),rep("6",3),rep("7",3),
       rep("4",3),rep("5",3),rep("6",3),rep("7",3),
       rep("4",3),rep("5",3),rep("6",3),rep("7",3))

pH = factor(ph)

CapH = c(rep("100-4",3), rep("100-5",3), rep("100-6",3), rep("100-7",3),
         rep("200-4",3), rep("200-5",3), rep("200-6",3), rep("200-7",3),
         rep("300-4",3), rep("300-5",3), rep("300-6",3), rep("300-7",3))
CapH = factor(CapH)

rawE = cbind(y,Ca,pH)
rawE = factor(rawE)
data_Effects = data.frame(rawE)


rawC = cbind(y,CapH)
rawC = factor(rawC)
data_CellMs = data.frame(rawC)


#Effects Model
modelE = lm(y~pH*Ca,data=data_Effects)
summary(aov(modelE))

#Cell Means Model
modelC = lm(y~CapH,data=data_CellMs)
summary(aov(modelC))

#Contrast matrix for linear, quad, and cubic trends

trendmatrix = matrix(c(-1, -1, -1, -1,  0,  0,  0,  0,  1, 1, 1, 1,
                        1,  1,  1,  1, -2, -2, -2, -2,  1, 1, 1, 1,
                       -3, -1,  1,  3, -3, -1,  1,  3, -3,-1, 1, 3,
                        1, -1, -1,  1,  1, -1, -1,  1,  1,-1,-1, 1,
                       -1,  3, -3,  1, -1,  3, -3,  1, -1, 3,-3, 1),12,5)

trendmatrix = t(trendmatrix)

rownames(trendmatrix) = c("Linear Trend Ca", "Quad Trend Ca", "Linear Trend pH", "Quad Trend pH", "Cubic Trend pH")

fit.contrast(modelC, "CapH", trendmatrix)

#Pairwise Comparisons of Ca and pH means:

lsmeans(modelC,"CapH")
#Comparisons with Unadjusted p-values
pairwise.t.test(y,CapH,p.adjust="none")
#Pairwise comparisons yielding Tukey Adjusted p-values

TukeyHSD(aov(y~CapH,data=data_CellMs))










