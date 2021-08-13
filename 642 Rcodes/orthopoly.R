x1 = rep(550,4) 
x2 = rep(600,4) 
x3 = rep(650,4)
x4 = rep(700,4)
x5 = rep(750,4)
x = c(x1,x2,x3,x4,x5)
y = c(6, 4, 5, 5, 32, 26, 24, 22, 45, 45, 44, 34, 63, 62, 44, 39, 87, 85, 72, 80) 

plot(x,y)

model = lm(y~x+I(x^2)+I(x^3)+I(x^4))

anova(model)

summary(model)







