
options(contrasts = c("contr.sum", "contr.poly"))

rw = c(19,20,21,24,26,22,25,25,19,25,27,21,24,24,31,32,33,32)

dose = c(rep("D1",9), rep("D2",9)) 

dosefac = factor(dose)

fung = c(rep("F1",3), rep("F2",2), rep("F3",4),
         rep("F1",2), rep("F2",3), rep("F3",4))

fungfac = factor(fung)

datafac = cbind(rw,dosefac,fungfac)
modelfac  = lm(rw~dosefac + fungfac + dosefac:fungfac)

#outputs Type I Sum of Squares
anova(modelfac)

#outputs Type III Sum of Squares

drop1(modelfac,.~.,test="F")

#Cell Means model

trt = c(rep("D1F1",3),rep("D1F2",2),rep("D1F3",4),
        rep("D2F1",2),rep("D2F2",3),rep("D2F3",4))

trtfac = factor(trt)
datatrt = cbind(rw,trtfac)

modeltrt  = lm(rw~trtfac)
anova(modeltrt)

#Pairwise Comparisons of Treatment means:

lsmeans(modeltrt,"trtfac")
#Comparisons with Unadjusted p-values
pairwise.t.test(rw,trtfac,p.adjust="none")
#Pairwise comparisons yielding Tukey Adjusted p-values

TukeyHSD(aov(rw~trtfac))






