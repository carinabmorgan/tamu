x <- c(1,2,3)
y1 <- c(5.57,6.2,NA)
y2 <- c(6.8 ,NA, 6.0)
y3 <- c(6.5  ,7.2,8.3)
y <-cbind(y1,y2,y3)

par(lab=c(3,15,4))
matplot(x,y,type="b",xlab="SURFACANT",ylab="MEAN SPECIFIC VOLUME",
        main="PROFILE PLOT WITH MISSING TREATMENTS",cex=.99,
        ylim=c(5,9),lab=c(3,15,4),pch=c("#$@"),xaxt="n",col="black")
axis(side=1,at=c(1,2,3),labels=c("SURF1","SURF2","SURF3"))
legend(1,9,pch=c("#$@"),legend=c("FAT=1","FAT=2","FAT=3"))
text(1.13,5.5,"FAT1")
text(1.13,6.4,"FAT3")
text(1.13,6.89,"FAT2")
