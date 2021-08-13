#POWER CURVES FOR NORMAL DISTRIBUTION

#Ho: mu0<=mu1, H1: mu0>mu1

mu0=8
mu1=seq(-2,12,.05)
delta1=-1.96+.5*(mu0-mu1)
pow1=pnorm(delta1)
par(lab=c(11,21,4))
plot(mu1,pow1,type="l",ylim=c(0,1),xlab=expression(mu),ylab="POWER")
text(6.1,.91, expression(H[o]))   
text(7.1,.91, expression( mu >=   8))   
text(6.1,.85,expression(H[1]))   
text(7.1,.85,expression( mu < 8))   
title("POWER FUNCTION")
out = cbind(mu1,pow1)

############
#######
############
#Ho: mu0>=mu1, H1: mu0<mu1

mu0 = 2
mu2 = seq(-2,14,.05)
delta2 = 1.28155 +.5*(mu0-mu2)
pow2 = 1-pnorm(delta2)
par(lab=c(11,21,4))
plot(mu2,pow2,type="l",ylim=c(0,1),xlab=expression(mu),ylab="POWER")
text(0.1,.91, expression(H[o]))   
text(1.4,.91, expression( mu <= 2))   
text(0.1,.85,expression(H[1]))   
text(1.4,.85,expression( mu > 2))   
title("POWER FUNCTION")
out = cbind(mu2,pow2)

##############
#######
##############
#Ho: mu0=mu1, H1: mu0 != mu1 (two sided)

mu0=7
mu3=seq(-4,16,.05)
delta3a= 1.96+.5*(mu0-mu3)
delta3b= -1.96+.5*(mu0-mu3)
pow3= pnorm(delta3b) + 1-pnorm(delta3a)
par(lab=c(15,21,4))
plot(mu3,pow3,type="l",ylim=c(0,1),xlab=expression(mu),ylab="POWER")
text(6.1, .9,expression(H[o] ))
text(7.5, .9,expression(mu == 7))   
text(6.1,.85,expression(H[1]))   
text(7.5,.85,expression(   mu != 7))   
title("POWER FUNCTION")
out3=cbind(mu3,pow3)

