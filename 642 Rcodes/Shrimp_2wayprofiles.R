
ts25 <-  c( 70.5,399.3,305.7)
ts35 <-  c(369.5,293.7,236.8)
ts <- cbind(ts25,ts35)
ds80 <-  c(239.2,370.2,301.0)
ds160 <- c(200.8,322.3,241.5)
ds <- cbind(ds80,ds160)
s <- c(10,25,40)
dt80 <-  c(298.3,308.6)
dt160 <- c(218.7,291.1)
dt <- cbind(dt80,dt160)
t <- c(25,35)

postscript("u:/meth2/psfiles/twowaysalintemp.ps",horizontal=FALSE)

matplot(s,ts,type="b",xlab="salinity Level %",ylab="Weight Gain (mg)",
        main="Profile Plot of Salinity by Temperature",cex=.99,col="black",
        ylim=c(0,500),lab=c(3,10,5),pch=c("#*"),xaxt="n")
axis(side=1,at=c(10,25,40),labels=c(10,25,40))
legend(10,500,pch=c("#*"),legend=c("TEMP=25","TEMP=35"))

postscript("u:/meth2/psfiles/twowaysalindensity.ps",horizontal=FALSE)

matplot(s,ds,type="b",xlab="salinity Level %",ylab="Weight Gain (mg)",
        main="Profile Plot of Salinity by Density",col="black",
        ylim=c(0,500),xlim=c(10,40),lab=c(3,10,5),pch=c("#*"),xaxt="n")
axis(side=1,at=c(10,25,40),labels=c(10,25,40))
legend(10,500,pch=c("#*"),legend=c("DEN=80","DEN=160"))

postscript("u:/meth2/psfiles/twowaydensitytemp.ps",horizontal=FALSE)

matplot(t,dt,type="b",xlab="Temperature (C)",ylab="Weight Gain (mg)",
        main="Profile Plot of Temperature by Density",col="black",
        ylim=c(0,500),xlim=c(25,35),lab=c(2,10,5),pch=c("#*"),xaxt="n")
axis(side=1,at=c(25,35),labels=c(25,35))
legend(25,500,pch=c("#*"),legend=c("DEN=80","DEN=160"))

graphics.off()

